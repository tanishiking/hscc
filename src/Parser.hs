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
            { Token.nestedComments = False
            , Token.reservedNames = ["if", "else", "while", "return", "int", "void"]
            , Token.reservedOpNames = []
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
  x <- many ExternalDeclaration
  return x
  <?> "parseProgram"


externalDeclaration :: Parser ExternalDeclaration
externalDeclaration =
  try (do 
    x <- declarationList
    return x)
  <|> do x <- functionPrototype
      return x
  <|> do x <- functionDefinition
      return x
  <?> "parseExternalDeclaration"


declaration :: Parser Declaration
declaration = do
  t <- typeSpecifier
  d <- declarationList
  _ <- semi
  return d
  <?> "declaration"


declarationList :: Parser DeclarationList
declarationList = do
  x <- sepBy declarator $ symbol ","
  return x
  <?> "declaration list"


declarator :: Parser Declarator
declarator =
  try (do
    
    )

  




