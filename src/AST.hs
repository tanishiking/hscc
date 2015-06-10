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

{-
 - 
 - ========================
 -  Data FunctionPrototype  
 - ========================
-}
data FunctionPrototype  = FunctionPrototype Type Identifier [(Type, Identifier)]
                        deriving (Show)
data FunctionDefinition = FunctionDefinition Type Identifier [(Type, Identifier)] [Stmt]
                        deriving (Show)


data Type = CInt
          | CVoid
          | CPointer Type
          deriving(Show)

data Stmt = EmptyStmt
          | ExprStmt     Expr
          | CompoundStmt [Stmt]
          | IfStmt       Expr Stmt Stmt
          | WhileStmt    Expr Stmt
          | ForStmt      Expr Expr Expr Stmt
          | ReturnStmt   Stmt
          deriving (Show)

data Expr = AssignExpr  Expr   Expr
          | UnaryPrim   String Expr
          | BinaryPrim  String Expr Expr
          | ArrayAccess Expr   Expr
          | ApplyFunc   String [Expr]
          | MultiExpr   [Expr]
          | Constant    Integer
          | IdentExpr   Identifier
          deriving (Show)
