{-# LANGUAGE RankNTypes #-}
module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String(Parser)
import Control.Monad
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
externalDeclaration = try (do 
    x <- declaration
    return x)
  <|> ( do 
    x <- functionPrototype
    return $ FuncProt x)
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
directDecl = try ( do
    name <- identifier
    size <- brackets natural
    return $ Sequence name size )
  <|> ( do
    name <- identifier
    return $ Variable name)


typeSpecifier :: Parser Type
typeSpecifier =  (symbol "int"  >> return CInt)
             <|> (symbol "void" >> return CVoid)

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


-- String '*', Identifier functionName,
funcDeclarator :: Parser (String, Identifier, [(Type, Identifier)])
funcDeclarator = do
  p      <- pointer
  fname  <- identifier
  params <- parens $ paramDeclaration `sepBy` (symbol ",")
  return $ (p, fname, params)


paramDeclaration :: Parser (Type, Identifier)
paramDeclaration = do
  t    <- typeSpecifier
  p    <- pointer
  name <- identifier
  return $ (checkPointer p t, name)

{-
 - ========================
 -   Function Definition
 - ========================

functionDef :: Parser FunctionDefinition
functionDef = do
  t                  <- typeSpecifier
  (p, fname, params) <- funcDeclarator
  stmts              <- 
-}







{-
 - =========================
 -  build expression parser
 - =========================

table :: [[Operator String () Identity Expr]]
table = [map op_prefix ["-", "&", "*"],
         map op_infix  ["*", "/"],
         map op_infix  ["+", "-"],
         map op_infix  ["<=", "<", ">", ">="],
         map op_infix  ["==", "!="],
         map op_infix  ["&&"],
         map op_infix  ["||"]]
      where
        op_func f s = do { return $ f s }
        op_prefix s = Prefix (op_func UnaryPrim s)
        op_infix  s = Infix  (op_func BinaryPrim s) AssocLeft
-}
