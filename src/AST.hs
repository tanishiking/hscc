module AST where

import Text.Parsec

type Program = [ExternalDeclaration]
type Identifier = String

data ExternalDeclaration = Decl     SourcePos DeclaratorList
                         | FuncProt SourcePos FunctionPrototype
                         | FuncDef  SourcePos FunctionDefinition
                         deriving (Show)

type DeclaratorList = [(Type, DirectDeclarator)]

data DirectDeclarator = Variable SourcePos Identifier
                      | Sequence SourcePos Identifier Integer
                      deriving (Show)

{-
 - ========================
 -  Data FunctionPrototype  
 - ========================
-}
data FunctionPrototype  = FunctionPrototype SourcePos Type Identifier [(Type, Identifier)]
                        deriving (Show)
data FunctionDefinition = FunctionDefinition SourcePos Type Identifier [(Type, Identifier)] CompoundStatement
                        deriving (Show)


data Type = CInt
          | CVoid
          | CPointer Type
          deriving(Show)

data Stmt = EmptyStmt    SourcePos
          | ExprStmt     SourcePos Expr
          | CompoundStmt SourcePos CompoundStatement
          | IfStmt       SourcePos Expr Stmt Stmt
          | WhileStmt    SourcePos Expr Stmt
          | ForStmt      SourcePos Expr Expr Expr Stmt
          | ReturnStmt   SourcePos Expr
          deriving (Show)


data CompoundStatement = CompoundStatement SourcePos DeclarationList [Stmt]
                       deriving (Show)

data DeclarationList = DeclarationList SourcePos [DeclaratorList]
                     deriving (Show)

data Expr = AssignExpr   SourcePos Expr       Expr
          | Or           SourcePos Expr       Expr 
          | And          SourcePos Expr       Expr
          | Equal        SourcePos Expr       Expr
          | NotEqual     SourcePos Expr       Expr
          | Lt           SourcePos Expr       Expr
          | Gt           SourcePos Expr       Expr
          | Lte          SourcePos Expr       Expr 
          | Gte          SourcePos Expr       Expr
          | Plus         SourcePos Expr       Expr
          | Minus        SourcePos Expr       Expr
          | Multiple     SourcePos Expr       Expr
          | Devide       SourcePos Expr       Expr
          | UnaryMinus   SourcePos Expr
          | UnaryAddress SourcePos Expr   
          | UnaryPointer SourcePos Expr
          | CallFunc     SourcePos String    [Expr]
          | ArrayAccess  SourcePos Expr       Expr
          | ExprList     SourcePos [Expr]
          | Constant     SourcePos Integer
          | IdentExpr    SourcePos Identifier
          deriving (Show)
