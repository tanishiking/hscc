module CheckedAST where

import Environment
import Text.Parsec

type CheckedProgram = [CheckedEDecl]

data CheckedEDecl = CheckedDecl     SourcePos [Info]
                  | CheckedFuncProt SourcePos  Info  [Info]
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
                 | CheckedDevide       SourcePos CheckedExpr CheckedExpr
                 | CheckedUnaryAddress SourcePos CheckedExpr
                 | CheckedUnaryPointer SourcePos CheckedExpr
                 | CheckedCallFunc     SourcePos Info        [CheckedExpr]
                 | CheckedExprList     SourcePos [CheckedExpr]
                 | CheckedConstant     SourcePos Integer
                 | CheckedIdentExpr    SourcePos Info
                 deriving (Show)
