{-# LANGUAGE RankNTypes #-}
module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String(Parser)
import Control.Monad
import Control.Monad.Identity(Identity)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

import AST

smallCStyle = javaStyle
            { P.nestedComments = False
            , P.reservedNames = ["if", "else", "while", "return", "int", "void"]
            , P.reservedOpNames = []
            }

lexer :: P.TokenParser()
lexer = P.makeTokenParser smallCStyle

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

lexeme :: forall a. Parser a -> Parser a
lexeme = P.lexeme lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

natural :: Parser Integer
natural = P.natural lexer

identifier :: Parser String
identifier = P.identifier lexer

semi :: Parser String
semi = P.semi lexer

comma :: Parser String
comma = P.comma lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: forall a. Parser a -> Parser a
parens = P.parens lexer

braces :: forall a. Parser a -> Parser a
braces = P.braces lexer

brackets :: forall a. Parser a -> Parser a
brackets = P.brackets lexer

{-
 - Parser
 -}

parseProgram :: Parser Program
parseProgram = do
  (P.whiteSpace lexer)
  x <- many externalDeclaration
  return x
  <?> "parseProgram"


externalDeclaration :: Parser ExternalDeclaration
externalDeclaration = try (do x <- declaration
                              return x)
                      <|> ( do x <- functionPrototype
                               return $ FuncProt x)
                      <|> ( do x <- functionDef
                               return $ FuncDef x)
                      <?> "parseExternalDeclaration"


declaration :: Parser ExternalDeclaration
declaration = do
  d <- declaratorList
  _ <- semi
  return $ Decl d
  <?> "declaration"


checkPointer :: String -> Type -> Type
checkPointer p t = 
  if p == "*" then CPointer t else t 

{- 
 - =====================================
 - int a, * b, c
 - (CInt a), (CPointer CInt b), (CInt c)
 - =====================================
-}

genDecl :: Type -> [(String, DirectDeclarator)] -> DeclaratorList
genDecl t str_decl = foldr f [] str_decl
  where f (p, direct) acc = (checkPointer p t, direct):acc


declaratorList :: Parser DeclaratorList
declaratorList = do
  t <- typeSpecifier
  x <- sepBy declarator $ symbol ","
  return (genDecl t x)
  <?> "declarator list"


pointer :: Parser String
pointer = option "" $ symbol "*"


declarator :: Parser (String, DirectDeclarator)
declarator = do
  p    <- pointer
  decl <- directDecl
  return (p, decl)
  <?> "declarator"


directDecl :: Parser DirectDeclarator
directDecl = try ( do name <- identifier
                      size <- brackets natural
                      return $ Sequence name size )
             <|> ( do name <- identifier
                      return $ Variable name)
             <?> "directDeclarator"


typeSpecifier :: Parser Type
typeSpecifier =  (symbol "int"  >> return CInt)
             <|> (symbol "void" >> return CVoid)
             <?> "typeSpecifier"

{-
 - ========================
 -    Function Prototype
 - ========================
 -}

functionPrototype :: Parser FunctionPrototype
functionPrototype = do
  t                  <- typeSpecifier
  (p, fname, params) <- funcDeclarator <* semi
  return $ FunctionPrototype (checkPointer p t) fname params
  <?> "FunctionPrototype"


-- String '*', Identifier functionName,
funcDeclarator :: Parser (String, Identifier, [(Type, Identifier)])
funcDeclarator = do
  p      <- pointer
  fname  <- identifier
  params <- parens $ paramDeclaration `sepBy` (symbol ",")
  return $ (p, fname, params)
  <?> "functionDeclarator"


paramDeclaration :: Parser (Type, Identifier)
paramDeclaration = do
  t    <- typeSpecifier
  p    <- pointer
  name <- identifier
  return $ (checkPointer p t, name)
  <?> "paramDeclaration"


functionDef :: Parser FunctionDefinition
functionDef = do
  t                  <- typeSpecifier
  (p, fname, params) <- funcDeclarator
  stmts              <- compoundStmt
  return $ FunctionDefinition (checkPointer p t) fname params stmts
  <?> "functionDefinition"

{-
 -  ===================
 -       Statement
 -  ===================
 -} 

compoundStmt :: Parser CompoundStatement
compoundStmt = braces ( do
  declList <- declarationList
  stmtList <- statementList
  return $ CompoundStatement declList stmtList)
  <?> "compound statement"


declarationList :: Parser DeclarationList
declarationList = do
  declarators <- declaratorList
  return $ DeclarationList declarators
  <?> "declarationList"


statementList :: Parser StatementList
statementList = do
  stmts <- many (try stmt)
  return $ StatementList stmts
  <?> "statementList"
  

stmt :: Parser Stmt
stmt =  try (semi >> return EmptyStmt)
    <|> try (do e <- expr <* semi
                return $ ExprStmt e)
    <|> try (do stmts <- compoundStmt
                return $ CompoundStmt stmts)
    <|> try (do symbol "if"
                e  <- parens expr
                s  <- stmt
                return $ IfStmt e s (EmptyStmt))
    <|> try (do e  <- symbol "if"   >> parens expr
                s1 <- stmt
                s2 <- symbol "else" >> stmt
                return $ IfStmt e s1 s2)
    <|> try (do e <- symbol "while" >> expr
                s <- stmt
                return $ WhileStmt e s)
    <|> try (do symbol "for"
                symbol "("
                dec    <- expr <* semi
                cond   <- expr <* semi
                update <- expr <* symbol ")"
                s      <- stmt
                return $ ForStmt dec cond update s)
    <|> try (do symbol "return"
                s <- stmt <* semi
                return $ ReturnStmt s)
    <?> "statement"


{-
 - =========================
 -          expr
 - =========================
 -}

expr :: Parser Expr
expr = ExprList <$> assignExpr `sepBy` comma
      <?> "expression"

assignExpr :: Parser Expr
assignExpr = try ( do 
    ident <- identifier
    symbol "="
    assign <- assignExpr
    return $ AssignExpr ident assign)
  <|> logicalOrExpr
  <?> "assignExpr"

logicalOrExpr :: Parser Expr
logicalOrExpr =  buildExpressionParser table unaryExpr
             <?> "logicalOrExpr"


{-
 - =========================
 -  build expression parser
 - =========================
 -}

table :: [[Operator String () Identity Expr]]
table = [[op "*" Multiple, op "/" Devide]
        ,[op "+" Plus,     op "-" Minus]
        ,[op "<" Lt,       op ">" Gt,
          op "<=" Lte,     op ">=" Gte]
        ,[op "==" Equal,   op "!=" NotEqual]
        ,[op "&&" And]
        ,[op "||" Or]]
  where op s f = Infix( do
                    reservedOp s
                    return f
                    <?> "unary expr")
                  AssocLeft


unaryExpr :: Parser Expr
unaryExpr =  try postFixExpr
         <|> ( do symbol "-"
                  p <- unaryExpr
                  return $ UnaryMinus p)
         <|> ( do symbol "&"
                  p <- unaryExpr
                  return $ UnaryAddress p)
         <|> ( do symbol "*"
                  p <- unaryExpr
                  return $ UnaryPointer p)
         <?> "unaryExpr"


postFixExpr :: Parser Expr
postFixExpr = try (do fname <- identifier
                      args  <- argExprList
                      return $ CallFunc fname args)
          <|> try (do array    <- expr
                      accessor <- (brackets expr)
                      return $ ArrayAccess array accessor)
          <|> primaryExpr
          <?> "postFixExpr"


argExprList :: Parser [Expr]
argExprList = try (assignExpr `sepBy` comma)
          <?> "expression"

primaryExpr :: Parser Expr
primaryExpr =  try (parens expr)
           <|> liftM Constant natural
           <|> liftM IdentExpr identifier
           <?> "primaryExpr"

