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
 - ========================
 -  Data FunctionPrototype  
 - ========================
-}
data FunctionPrototype  = FunctionPrototype Type Identifier [(Type, Identifier)]
                        deriving (Show)
data FunctionDefinition = FunctionDefinition Type Identifier [(Type, Identifier)] CompoundStatement
                        deriving (Show)


data Type = CInt
          | CVoid
          | CPointer Type
          deriving(Show)

data Stmt = EmptyStmt
          | ExprStmt     Expr
          | CompoundStmt CompoundStatement
          | IfStmt       Expr Stmt Stmt
          | WhileStmt    Expr Stmt
          | ForStmt      Expr Expr Expr Stmt
          | ReturnStmt   Expr
          deriving (Show)


data CompoundStatement = CompoundStatement DeclarationList [Stmt]
                       deriving (Show)

data DeclarationList = DeclarationList [DeclaratorList]
                     deriving (Show)

data Expr = AssignExpr   Expr       Expr
          | Or           Expr       Expr 
          | And          Expr       Expr
          | Equal        Expr       Expr
          | NotEqual     Expr       Expr
          | Lt           Expr       Expr
          | Gt           Expr       Expr
          | Lte          Expr       Expr 
          | Gte          Expr       Expr
          | Plus         Expr       Expr
          | Minus        Expr       Expr
          | Multiple     Expr       Expr
          | Devide       Expr       Expr
          | UnaryMinus   Expr
          | UnaryAddress Expr   
          | UnaryPointer Expr
          | CallFunc     String    [Expr]
          | ArrayAccess  Expr       Expr
          | ExprList    [Expr]
          | Constant     Integer
          | IdentExpr    Identifier
          deriving (Show)
