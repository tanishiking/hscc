module CheckedAST where

import Environment
import Text.Parsec

type CheckedProgram = [CheckedEDecl]

data CheckedEDecl = CheckedDecl     SourcePos [Info]
--                | CheckedFuncProt SourcePos  Info  [Info]
                  | CheckedFuncDef  SourcePos  Info  [Info] CheckedStmt
                  deriving (Show)

data CheckedStmt = CheckedEmptyStmt
                 | CheckedExprStmt      CheckedExpr
                 | CheckedCompoundStmt [Info] [CheckedStmt]
                 | CheckedIfStmt       SourcePos CheckedExpr CheckedStmt CheckedStmt
                 | CheckedWhileStmt    SourcePos CheckedExpr CheckedStmt
                 | CheckedReturnStmt   SourcePos CheckedExpr
                 deriving (Show)


data CheckedExpr = CheckedAssignExpr   SourcePos CheckedExpr CheckedExpr
                 | CheckedOr           SourcePos CheckedExpr CheckedExpr
                 | CheckedAnd          SourcePos CheckedExpr CheckedExpr
                 | CheckedEqual        SourcePos CheckedExpr CheckedExpr
                 | CheckedNotEqual     SourcePos CheckedExpr CheckedExpr
                 | CheckedLt           SourcePos CheckedExpr CheckedExpr
                 | CheckedGt           SourcePos CheckedExpr CheckedExpr
                 | CheckedLte          SourcePos CheckedExpr CheckedExpr
                 | CheckedGte          SourcePos CheckedExpr CheckedExpr
                 | CheckedPlus         SourcePos CheckedExpr CheckedExpr
                 | CheckedMinus        SourcePos CheckedExpr CheckedExpr
                 | CheckedMultiple     SourcePos CheckedExpr CheckedExpr
                 | CheckedDivide       SourcePos CheckedExpr CheckedExpr
                 | CheckedUnaryAddress SourcePos CheckedExpr
                 | CheckedUnaryPointer SourcePos CheckedExpr
                 | CheckedCallFunc     SourcePos Info        [CheckedExpr]
                 | CheckedExprList     SourcePos [CheckedExpr]
                 | CheckedConstant     SourcePos Integer
                 | CheckedIdentExpr    SourcePos Info

instance Show CheckedExpr where
  show (CheckedAssignExpr _ e1 e2)  = show e1 ++ " = "  ++ show e2
  show (CheckedOr _ e1 e2)      = show e1 ++ " || " ++ show e2
  show (CheckedAnd _ e1 e2)     = show e1 ++ " && " ++ show e2
  show (CheckedEqual _ e1 e2) = show e1 ++ " == " ++ show e2
  show (CheckedNotEqual _ e1 e2) = show e1 ++ " != " ++ show e2
  show (CheckedLt _ e1 e2) = show e1 ++ " < " ++ show e2
  show (CheckedGt _ e1 e2) = show e1 ++ " > " ++ show e2
  show (CheckedLte _ e1 e2) = show e1 ++ " <= " ++ show e2
  show (CheckedGte _ e1 e2) = show e1 ++ " >= " ++ show e2
  show (CheckedPlus _ e1 e2) = show e1 ++ " + " ++ show e2
  show (CheckedMinus _ e1 e2) = show e1 ++ " - " ++ show e2
  show (CheckedMultiple _ e1 e2) = show e1 ++ " * " ++ show e2
  show (CheckedDivide _ e1 e2) = show e1 ++ " / " ++ show e2
  show (CheckedUnaryAddress _ expr) = show expr
  show (CheckedUnaryPointer _ expr) = show expr
  show (CheckedCallFunc _ info _) = fst info
  show (CheckedExprList _ exprs)  = show exprs
  show (CheckedConstant _ num)    = show num
  show (CheckedIdentExpr _ info)  = fst info
