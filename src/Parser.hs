{-# LANGUAGE RankNTypes #-}
module Parser where

import Debug.Trace

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.Parsec.String(Parser)
import Control.Monad
import Control.Monad.Identity(Identity)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (javaStyle)

import AST

smallCStyle = javaStyle
            { P.nestedComments = False
            , P.reservedNames = ["if", "else", "while", "return", "int", "void"]
            , P.reservedOpNames = ["*", "/", "+", "-", ">", "<", "<=", ">=", "==", "!=", "&&", "||", "="]
            }

lexer :: P.TokenParser()
lexer = P.makeTokenParser smallCStyle

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

lexeme :: forall a. Parser a -> Parser a
lexeme = P.lexeme lexer

identifier :: Parser String
identifier = P.identifier lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

natural :: Parser Integer
natural = P.natural lexer

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

{- =========
 -  Parser
   ========-}

parseProgram :: String -> Either String Program
parseProgram input = case parse program "Parse" input of
                     Left  err -> Left $ show err
                     Right val -> Right val


program :: Parser Program
program = do
  x <- many1 externalDeclaration
  return x
  <?> "program"


externalDeclaration :: Parser ExternalDeclaration
externalDeclaration = try (do x <- declaration
                              return x)
                  <|> try ( do pos                <- getPosition
                               t                  <- typeSpecifier
                               (p, fname, params) <- funcDeclarator <* semi
                               return $ FuncProt pos (checkPointer p t) fname params)
                  <|> (do pos <- getPosition
                          t    <- typeSpecifier
                          (p, fname, params) <- funcDeclarator
                          s    <- stmt
                          return $ FuncDef pos (checkPointer p t) fname params s)
                  <?> "parseExternalDeclaration"


declaration :: Parser ExternalDeclaration
declaration = do
  pos <- getPosition
  d   <- declaratorList <* semi
  return $ Decl pos d
  <?> "declaration"


checkPointer :: String -> Type -> Type
checkPointer p t = 
  if p == "*" then CPointer t else t 

{- =====================================
 - int a, * b, c
 - (CInt a), (CPointer CInt b), (CInt c)
 - ===================================== -}

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
directDecl = try ( do pos  <- getPosition
                      name <- identifier
                      size <- brackets natural
                      return $ Sequence pos name size )
             <|> ( do pos  <- getPosition
                      name <- identifier
                      return $ Variable pos name)
             <?> "directDeclarator"


typeSpecifier :: Parser Type
typeSpecifier =  (symbol "int"  >> return CInt)
             <|> (symbol "void" >> return CVoid)
             <?> "typeSpecifier"


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



{-  ===================
 -       Statement
 -  =================== -} 

compoundStmt :: Parser Stmt
compoundStmt = do
  pos      <- getPosition
  _        <- symbol "{"
  declList <- many $ try (declaratorList <* semi)
  stmtList <- many $ try stmt
  _        <- symbol "}"
  return $ CompoundStmt pos (concat declList) stmtList
  <?> "compound statement"


stmt :: Parser Stmt
stmt =  try (do pos <- getPosition
                _   <- semi
                return $ EmptyStmt pos)
    <|> try (do e   <- expr <* semi
                pos <- getPosition
                return $ ExprStmt pos e)
    <|> try compoundStmt
    <|> try (do e   <- symbol "if"   >> parens expr
                pos <- getPosition
                s1  <- stmt
                s2  <- symbol "else" >> stmt
                return $ IfStmt pos e s1 s2)
    <|> try (do symbol "if"
                pos <- getPosition
                e   <- parens expr
                s   <- stmt
                return $ IfStmt pos e s (EmptyStmt pos))
    <|> try (do e   <- symbol "while" >> parens expr
                pos <- getPosition
                s   <- stmt
                return $ WhileStmt pos e s)
    <|> try (do symbol "for"
                symbol "("
                pos    <- getPosition
                dec    <- expr <* semi
                cond   <- expr <* semi
                update <- expr <* symbol ")"
                s      <- stmt
--              return $ ForStmt pos dec cond update s)
                return $ CompoundStmt pos 
                                      [] 
                                      [ExprStmt pos dec, 
                                       WhileStmt pos cond (CompoundStmt pos 
                                                                        []
                                                                        [s, ExprStmt pos update])])
    <|> try (do symbol "return"
                pos <- getPosition
                e   <- expr <* semi
                return $ ReturnStmt pos e)
    <?> "statement"


expr :: Parser Expr
expr = liftM2 ExprList getPosition (assignExpr `sepBy` comma)
    <?> "expression"

assignExpr :: Parser Expr
assignExpr = try ( do 
    pos    <- getPosition
    dest   <- logicalOrExpr 
    symbol "="
    assign <- assignExpr
    return $ AssignExpr pos dest assign)
  <|>  logicalOrExpr
  <?> "assignExpr"

logicalOrExpr :: Parser Expr
logicalOrExpr =  buildExpressionParser table unaryExpr
             <?> "logicalOrExpr"


{- =========================
 -  build expression parser
 - ========================= -}

table :: [[Operator String () Identity Expr]]
table = [[op "*"  Multiple AssocLeft, op "/"  Divide   AssocLeft]
        ,[op "+"  Plus     AssocLeft, op "-"  Minus    AssocLeft]
        ,[op "<"  Lt       AssocLeft, op ">"  Gt       AssocLeft,
          op "<=" Lte      AssocLeft, op ">=" Gte      AssocLeft]
        ,[op "==" Equal    AssocLeft, op "!=" NotEqual AssocLeft]
        ,[op "&&" And      AssocLeft]
        ,[op "||" Or       AssocLeft]]
  where op s f = Infix(do {
                        pos <- getPosition;
                        _   <- reservedOp s;
                        return $ f pos;} 
                        <?> "table")
                      


unaryExpr :: Parser Expr
unaryExpr =  try postFixExpr
         <|> try ( do symbol "-"
                      pos <- getPosition
                      p   <- unaryExpr
--                    return $ UnaryMinus pos p)
                      return $ Minus pos (Constant pos 0) p) 
         <|> try ( do symbol "&"
                      pos <- getPosition
                      p   <- unaryExpr
                      return $ UnaryAddress pos p)
         <|> try ( do symbol "*"
                      pos <- getPosition
                      p   <- unaryExpr
                      return $ UnaryPointer pos p)
         <?> "unaryExpr"


postFixExpr :: Parser Expr
postFixExpr = try (do pos   <- getPosition
                      fname <- identifier
                      args  <- parens argExprList
                      return $ CallFunc pos fname args)
          <|> try (do arrayName    <- primaryExpr
                      arrayAccess  <- arrayAccessExpr arrayName
                      return arrayAccess)
          <|> primaryExpr
          <?> "postFixExpr"

arrayAccessExpr :: Expr -> Parser Expr
arrayAccessExpr left = do
  accessor <- many1 $ liftTM getPosition (brackets expr)
  return $ foldl (\acc (pos, i) -> UnaryPointer pos (Plus pos acc i)) left accessor
  <?> "arrayAccessExpr"

argExprList :: Parser [Expr]
argExprList = try (assignExpr `sepBy` comma)
          <?> "expression"

primaryExpr :: Parser Expr
primaryExpr =  try (parens expr)
           <|> try (liftM2 Constant getPosition natural)
           <|> liftM2 IdentExpr getPosition identifier
           <?> "primaryExpr"


liftTM :: Monad m => m a -> m b -> m (a, b)
liftTM ma mb = do
  a <- ma
  b <- mb
  return (a, b)
