module GenIntermed where

import Intermed
import CheckedAST
import Environment

import Debug.Trace

import Control.Monad.State.Strict

type VarNum   = Int
type Reusable = [VarNum]
type VarEnv   = State (Reusable, VarNum)

runVarEnv :: VarEnv a -> a
runVarEnv v = evalState v ([], 0)

result :: [IVar] -> IVar
result [] = error "unexpected vars"
result l  = head l


{- 新しく変数を作って次に出てくる変数の番号をひとつ増やす -}
genFreshVar :: VarEnv IVar
genFreshVar = do
  (reusables, varnum) <- get
  case reusables of
    []     -> do put ([], varnum+1) 
                 return ("tmp" ++ show varnum,
                         (Var, ChVoid, globalLevel))
    (n:ns) -> do put (ns, varnum) -- 再利用可能な変数を使う
                 return ("tmp" ++ show n,
                         (Var, ChVoid, globalLevel))

{-=========================================
 -      intermed expression converter
 =========================================-}

intermedProgram :: CheckedProgram -> IProgram
intermedProgram prog = runVarEnv $ mapM intermedExDecl prog

intermedExDecl :: CheckedEDecl -> VarEnv IExDecl
intermedExDecl (CheckedDecl _ vars) = return $ IDecl vars
--intermedExDecl (CheckedFuncProt _ _ _) = return ()
intermedExDecl (CheckedFuncDef _ finfo args body) = do
  (vars, stmts) <- intermedStmt body
  return $ IFuncDef finfo args (ICompoundStmt [] stmts)


intermedStmt :: CheckedStmt -> VarEnv ([IVar], [IStmt])
intermedStmt (CheckedEmptyStmt) = return ([], [IEmptyStmt])
intermedStmt (CheckedExprStmt e) = intermedExpr e
intermedStmt (CheckedCompoundStmt decls stmts) = do
  istmts <- mapM intermedStmt stmts
  let varsComp  = concat $ map fst istmts
      stmtsComp = concat $ map snd istmts
  return (varsComp, [ICompoundStmt decls stmtsComp])
intermedStmt (CheckedIfStmt _ cond true false) = do
  (varsCond, stmtCond)   <- intermedExpr cond
  (varsTrue, stmtTrue)   <- intermedStmt true
  (varsFalse, stmtFlase) <- intermedStmt false
  return (varsCond ++ varsTrue ++ varsFalse,
          stmtCond ++ [IIfStmt (result varsCond) stmtTrue stmtFlase])
intermedStmt (CheckedWhileStmt _ cond body) = do
  (varsCond, stmtCond) <- intermedExpr cond
  (varsBody, stmtBody) <- intermedStmt body
  return (varsCond ++ varsBody,
          stmtCond ++ [IWhileStmt (result varsCond) stmtBody])
intermedStmt (CheckedReturnStmt _ expr) = do
  (varsReturn, stmtReturn) <- intermedExpr expr
  return (varsReturn, stmtReturn ++ [IReturnStmt $ result varsReturn])


intermedExpr :: CheckedExpr -> VarEnv ([IVar], [IStmt])
intermedExpr (CheckedAssignExpr _ dest src) =
  case dest of 
    (CheckedUnaryPointer _ dst) -> do 
      (varsDest, stmtDest) <- intermedExpr dest
      (varsSrc,  stmtSrc)  <- intermedExpr src
      return (varsDest ++ varsSrc,
              stmtDest ++ stmtSrc ++ [IWriteStmt (result varsDest)
                                                 (result varsSrc)])
    (CheckedIdentExpr _ varDst) -> do
      (varsSrc, stmtSrc) <- intermedExpr src
      return (varsSrc,
              stmtSrc ++ [ILetStmt varDst (IVarExpr $ result varsSrc)])
intermedExpr (CheckedOr _ e1 e2)       = intermedOrExpr e1 e2
intermedExpr (CheckedAnd _ e1 e2)      = intermedAndExpr e1 e2
intermedExpr (CheckedEqual _ e1 e2)    = intermedRelopExpr "=" e1 e2
intermedExpr (CheckedNotEqual _ e1 e2) = intermedRelopExpr "!=" e1 e2
intermedExpr (CheckedLt _ e1 e2)       = intermedRelopExpr "<" e1 e2
intermedExpr (CheckedLte _ e1 e2)      = intermedRelopExpr "<=" e1 e2
intermedExpr (CheckedGt _ e1 e2)       = intermedRelopExpr ">" e1 e2
intermedExpr (CheckedGte _ e1 e2)      = intermedRelopExpr ">=" e1 e2
intermedExpr (CheckedPlus _ e1 e2)     = intermedAopExpr "+" e1 e2
intermedExpr (CheckedMinus _ e1 e2)    = intermedAopExpr "-" e1 e2
intermedExpr (CheckedMultiple _ e1 e2) = intermedAopExpr "*" e1 e2
intermedExpr (CheckedDivide _ e1 e2)   = intermedAopExpr "/" e1 e2
intermedExpr (CheckedUnaryPointer _ e) = do
  dest <- genFreshVar
  (vars, stmts) <- intermedExpr e
  return (dest:vars, stmts ++ [IReadStmt dest (result vars)])
intermedExpr (CheckedUnaryAddress _ e) = do
  dest <- genFreshVar
  (vars, stmts) <- intermedExpr e
  return (dest:vars, stmts ++ [ILetStmt dest (IAddrExpr (result vars))])
intermedExpr (CheckedCallFunc _ func args) = do
  res <- trace (show args) mapM intermedExpr args
  if fst func == "print"
    then let [(vars, stmts)] = res in
         return (vars, stmts ++ [IPrintStmt $ result vars])
    else let resArgs  = map (head . fst) res
             resVars  = concat $ map fst res
             resStmts = concat $ map snd res in do
         dest <- genFreshVar
         return (dest:resVars,
                 resStmts ++ [ICallStmt dest func resArgs])
intermedExpr (CheckedExprList _ exprs) = do
  [(vars, stmts)] <- mapM intermedExpr exprs
  return (vars, stmts)
intermedExpr (CheckedConstant _ num) = do
  dest <- genFreshVar
  return ([dest], [ILetStmt dest (IIntExpr num)])
intermedExpr (CheckedIdentExpr _ ident) = return ([ident], [])


intermedOrExpr :: CheckedExpr -> CheckedExpr -> VarEnv ([IVar], [IStmt])
intermedOrExpr e1 e2 = do
  (vars1, stmts1) <- intermedExpr e1
  (vars2, stmts2) <- intermedExpr e2
  dest            <- genFreshVar
  return (dest:(vars1 ++ vars2),
          stmts1 ++ stmts2 ++ 
            [IIfStmt (result vars1) [ILetStmt dest (IIntExpr 1)]
                                    [IIfStmt (result vars2) [ILetStmt dest (IIntExpr 1)]
                                                            [ILetStmt dest (IIntExpr 0)]]])


intermedAndExpr :: CheckedExpr -> CheckedExpr -> VarEnv ([IVar], [IStmt])
intermedAndExpr e1 e2 = do
  (vars1, stmts1) <- intermedExpr e1
  (vars2, stmts2) <- intermedExpr e2
  dest            <- genFreshVar
  return (dest:(vars1 ++ vars2),
          stmts1 ++ stmts2 ++
            [IIfStmt (result vars1) [IIfStmt (result vars2) [ILetStmt dest (IIntExpr 1)]
                                                            [ILetStmt dest (IIntExpr 0)]]
                                    [ILetStmt dest (IIntExpr 0)]])

intermedAopExpr :: String -> 
                   CheckedExpr -> 
                   CheckedExpr ->
                   VarEnv ([IVar], [IStmt])
intermedAopExpr op e1 e2 = do
  (vars1, stmts1) <- intermedExpr e1
  (vars2, stmts2) <- intermedExpr e2
  dest            <- genFreshVar
  return (dest:(vars1 ++ vars2),
          stmts1 ++ stmts2 ++ 
            [ILetStmt dest $ IAopExpr op (result vars1) (result vars2)])



intermedRelopExpr :: String -> 
                     CheckedExpr -> 
                     CheckedExpr -> 
                     VarEnv ([IVar], [IStmt])
intermedRelopExpr op e1 e2 = do
  (vars1, stmts1) <- intermedExpr e1
  (vars2, stmts2) <- intermedExpr e2
  dest            <- genFreshVar
  return (dest:(vars1 ++ vars2),
          stmts1 ++ stmts2 ++ 
            [ILetStmt dest $ IRelopExpr op (result vars1) (result vars2)])


