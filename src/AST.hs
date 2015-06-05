module AST where

type Program = [ExternalDeclaration]
type Identifier = String

data ExternalDeclaration = Decl     DeclaratorList
                         | FuncProt FunctionPrototype
                         | FuncDef  FunctionDefinition
                         deriving (Show)

type DeclaratorList = [(Type, DirectDeclarator)]

data DirectDeclarator = Variable Identifier
                      | Sequence Identifier Integer
                      deriving (Show)

-- === Data FunctionPrototype === 
type FunctionPrototype  = String
type FunctionDefinition = String


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


data Expr = Constant  Integer
          | IdentExpr Identifier
          deriving (Show)
