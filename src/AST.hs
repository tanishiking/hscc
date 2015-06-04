module AST where

data Program = [ExternalDeclaration] deriving (Show)
data Identifier = String deriving (Show)

data ExternalDeclaration = Decl     Declaration
                         | FuncProt FunctionPrototype
                         | FuncDef  FunctionDefinition
                         deriving (Show)

data Declaration = Type DeclaratorList deriving (Show)

data DeclaratorList = [Declarator] deriving (Show)

data Declarator = Variable Identifier
                | Sequence Identifier Integer
                deriving (Show)

-- === Data FunctionPrototype === 
-- === Function Definition===


data Type = CInt
          | CVoid
          | CPointer Type
          deriving(Show)

data Stmt = EmptyStmt
          | ExprStmt     Expr
          | CompoundStmt [Expr]
          | IfStmt       Expr Stmt Stmt
          | WhileStmt    Expr Stmt
          | ForStmt      Expr Expr Expr Stmt
          | ReturnStmt   Stmt
          deriving (Show)

data Expr = 

