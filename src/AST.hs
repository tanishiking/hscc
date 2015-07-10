module AST where

import Text.Parsec

type Program = [ExternalDeclaration]

--type Identifier = String

type Identifier = String

data ExternalDeclaration = Decl     SourcePos DeclaratorList
                         | FuncProt SourcePos Type Identifier [(Type, Identifier)]
                         | FuncDef  SourcePos Type Identifier [(Type,Identifier)] Stmt
                         deriving (Show)

type DeclaratorList = [(Type, DirectDeclarator)]

data DirectDeclarator = Variable SourcePos Identifier
                      | Sequence SourcePos Identifier Integer

instance Show DirectDeclarator where
  show (Variable _ ident)     = show ident
  show (Sequence _ ident int) = concat [show ident, "[", show int, "]"]

{-===============
 -    Type
 ================-}
data Type = CInt
          | CVoid
          | CPointer Type
          deriving(Eq)

instance Show Type where
  show (CPointer CInt) = "int *"
  show (CInt)          = "int "
  show (CVoid)         = "void "
  show _               = error "invalid type"



data Stmt = EmptyStmt    SourcePos
          | ExprStmt     SourcePos Expr
          | CompoundStmt SourcePos DeclaratorList [Stmt]
          | IfStmt       SourcePos Expr Stmt Stmt
          | WhileStmt    SourcePos Expr Stmt
          | ReturnStmt   SourcePos Expr
          deriving (Show)


--data CompoundStatement = CompoundStatement SourcePos DeclarationList [Stmt]
--                       deriving (Show)

--data DeclarationList = DeclarationList SourcePos [DeclaratorList]
--                     deriving (Show)

data Expr = AssignExpr   SourcePos Expr        Expr
          | Or           SourcePos Expr        Expr 
          | And          SourcePos Expr        Expr
          | Equal        SourcePos Expr        Expr
          | NotEqual     SourcePos Expr        Expr
          | Lt           SourcePos Expr        Expr
          | Gt           SourcePos Expr        Expr
          | Lte          SourcePos Expr        Expr 
          | Gte          SourcePos Expr        Expr
          | Plus         SourcePos Expr        Expr
          | Minus        SourcePos Expr        Expr
          | Multiple     SourcePos Expr        Expr
          | Divide       SourcePos Expr        Expr
          | UnaryAddress SourcePos Expr   
          | UnaryPointer SourcePos Expr
          | CallFunc     SourcePos Identifier [Expr]
          | ExprList     SourcePos [Expr]
          | Constant     SourcePos Integer
          | IdentExpr    SourcePos Identifier
          deriving (Show)

