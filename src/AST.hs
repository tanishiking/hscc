module AST where

import Text.Parsec

type Program = [ExternalDeclaration]

--type Identifier = String

type Level = Integer

data Identifier = IdentBefore   String
                | IdentGlobal   String Level
                | IdentVarDecl  String Level Integer
                | IdentParam    String Level Integer
                | IdentFuncProt String Level Integer
                | IdentFuncDef  String Level Integer
                | IdentVarRef   String Level Integer
                | IdentFuncCall String Level
                deriving (Eq)

instance Show Identifier where
  show (IdentBefore s)        = s
  show (IdentGlobal s v)      = s ++ ":" ++ show v
  show (IdentVarDecl s v1 v2) = concat [s, ":", show v1, ":", show v2]
  show (IdentParam s v1 v2)   = concat [s, ":", show v1, ":", show v2]
  show (IdentFuncDef s v _)   = s ++ ":" ++ show v
  show (IdentVarRef s v _)    = s ++ ":" ++ show v
  show (IdentFuncCall s v)    = s ++ ":" ++ show v


data ExternalDeclaration = Decl     SourcePos DeclaratorList
                         | FuncProt SourcePos FunctionPrototype
                         | FuncDef  SourcePos FunctionDefinition
                         deriving (Show)

type DeclaratorList = [(Type, DirectDeclarator)]

data DirectDeclarator = Variable SourcePos Identifier
                      | Sequence SourcePos Identifier Integer

instance Show DirectDeclarator where
  show (Variable _ ident)     = show ident
  show (Sequence _ ident int) = concat [show ident, "[", show int, "]"]

{- ========================
 -  Data FunctionPrototype  
 - ========================-}
data FunctionPrototype  = FunctionPrototype SourcePos Type Identifier [(Type, Identifier)]
                        deriving (Show)
data FunctionDefinition = FunctionDefinition SourcePos Type Identifier [(Type, Identifier)] Stmt
                        deriving (Show)


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
          | CompoundStmt SourcePos [DeclaratorList] [Stmt]
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
          | Devide       SourcePos Expr        Expr
--        | UnaryMinus   SourcePos Expr
          | UnaryAddress SourcePos Expr   
          | UnaryPointer SourcePos Expr
          | CallFunc     SourcePos Identifier [Expr]
--        | ArrayAccess  SourcePos Expr        Expr
          | ExprList     SourcePos [Expr]
          | Constant     SourcePos Integer
          | IdentExpr    SourcePos Identifier
          deriving (Show)
