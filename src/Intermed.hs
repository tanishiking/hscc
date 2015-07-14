module Intermed where

import Environment

type FpAddr = Int
type GpAddr = Int
type SpAddr = Int

data Address = Fp  Int
             | Gp  Int
             | Reg Int
             deriving (Show)

type IVarAddr = Address

type IProgram = [IExDecl]
data IVar = VarInfo Info
          | VarAddr IVarAddr
          deriving (Show)

data IExDecl = IDecl    [IVar]
             | IFuncDef IVar [IVar] IStmt
             deriving (Show)

data IStmt = IEmptyStmt
           | ILetStmt       IVar   IExpr
           | IWriteStmt     IVar   IVar
           | IReadStmt      IVar   IVar
           | IIfStmt        IVar   [IStmt]   [IStmt]
           | IWhileStmt     IVar   [IStmt]
           | ICallStmt      IVar   Info    [IVar]  
           | IReturnStmt    IVar
           | IPrintStmt     IVar
           | ICompoundStmt [IVar] [IStmt]
           deriving (Show)

data IExpr = IVarExpr   IVar
           | IIntExpr   Integer
           | IAopExpr   String IVar IVar --op arithmetic
           | IRelopExpr String IVar IVar --op compare
           | IAddrExpr  IVar
           deriving (Show)

