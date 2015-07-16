module AssignAddr where

import Intermed
import Environment

import Debug.Trace
import Data.Maybe
import Control.Monad.State

type AddrEnv = State (FpAddr, GpAddr, [(IVar, IVarAddr)])

getFpAddr :: IVar -> Int
getFpAddr (VarAddr (Fp n))  = n
getFpAddr (VarAddr (Gp n))  = 0
--getFpAddr (VarAddr (Reg n)) = 0
getFpAddr (VarInfo _      ) = 0


wordSize :: Int
wordSize = 4


-- 型によって確保すべき語数を返す
calcWords :: ChType -> Int 
calcWords ty = case ty of
  (ChInt)          -> 1
  (ChPointer ty)   -> calcWords ty
  (ChArray _ size) -> (fromIntegral size) :: Int
  (ChTmp)          -> 1
  otherType        -> error $ "invalid type :" ++ (show otherType)


-- グローバル宣言の数だけglobal pointerを動かして空間を確保
-- 返すのは一つ目の宣言された変数を指し示すポインタ
assignGpWords :: Int -> AddrEnv IVarAddr
assignGpWords numOfDecls = do
  (fp, gp, vars) <- get
  let newGp = gp + numOfDecls * wordSize
  put (fp, newGp, vars)
  return $ Gp (newGp - wordSize)


assignFpWords :: Int -> AddrEnv IVarAddr
assignFpWords numOfDecls = do
  (fp, gp, vars) <- get
  let newFp = fp - numOfDecls * wordSize
  put (newFp, gp, vars)
  return $ Fp (newFp + wordSize)


assignArgs :: [IVar] -> AddrEnv [IVarAddr]
assignArgs args = liftM fst (foldM f ([], 0) args)
  where f (acc, addr) ivar = do let fpAddr = Fp addr
                                pushVar (ivar, fpAddr)
                                return (fpAddr:acc, addr+wordSize)


-- 変数とそのアドレスを実際にメモリ内にpushする
pushVar :: (IVar, IVarAddr) -> AddrEnv ()
pushVar (var, addr) = do
  (fp, gp, vars) <- get
  put (fp, gp, (var, addr):vars)


resetFp :: Int -> AddrEnv ()
resetFp n = do
  (fp, gp, vars) <- get
  put (n, gp, vars)


getFp :: AddrEnv FpAddr
getFp = do
  (fp, gp, vars) <- get
  return fp


getAddr :: IVar -> AddrEnv IVar
getAddr ivar = do
  (fp, gp, vars) <- get
  return $ VarAddr (fromJust $ lookup ivar vars)


{-===============================
 -       Assign Address
 -===============================-}
assignAddr :: IProgram -> IProgram
assignAddr = runAddrEnv . assignIProgram


runAddrEnv :: AddrEnv IProgram -> IProgram
runAddrEnv s = evalState s (-wordSize, -wordSize, [])


assignIProgram :: IProgram -> AddrEnv IProgram
assignIProgram = mapM assignIExDecl


assignIExDecl :: IExDecl -> AddrEnv IExDecl
assignIExDecl (IDecl vars) =
  let requiredWords = map (calcWords . getTypeFromIVar) vars in do
    varsAddress <- mapM assignGpWords requiredWords
    mapM_ pushVar $ zip vars varsAddress
    return $ IDecl (map VarAddr varsAddress)
assignIExDecl (IFuncDef func args body) = do
  resetFp (-wordSize)
  args' <- assignArgs args
  addressedBody <- assignIStmt body
  return $ IFuncDef func (map VarAddr args') addressedBody


assignIStmt :: IStmt -> AddrEnv IStmt
assignIStmt (IEmptyStmt)                = return IEmptyStmt
assignIStmt (ILetStmt dest src)         = liftM2 ILetStmt   (getAddr dest) (assignIExpr src)
assignIStmt (IWriteStmt dest src)       = liftM2 IWriteStmt (getAddr dest) (getAddr src)
assignIStmt (IReadStmt dest src)        = liftM2 IReadStmt  (getAddr dest) (getAddr src)
assignIStmt (IIfStmt cond true false)   = liftM3 IIfStmt    (getAddr cond) (mapM assignIStmt true) (mapM assignIStmt false)
assignIStmt (IWhileStmt cond body)      = liftM2 IWhileStmt (getAddr cond) (mapM assignIStmt body)
assignIStmt (ICallStmt dest func args)  = liftM3 ICallStmt  (getAddr dest) (return func) (mapM getAddr args)
assignIStmt (IReturnStmt var)           = liftM IReturnStmt (getAddr var)
assignIStmt (IPrintStmt var)            = liftM IPrintStmt  (getAddr var)
assignIStmt (ICompoundStmt decls stmts) = do
  fp <- getFp
  let requiredWords = map (calcWords . getTypeFromIVar) decls
  declsAddress <- mapM assignFpWords requiredWords
  mapM_ pushVar $ zip decls declsAddress
  stmts' <- mapM assignIStmt stmts
  resetFp fp
  return $ ICompoundStmt (map VarAddr declsAddress) stmts'


assignIExpr :: IExpr -> AddrEnv IExpr
assignIExpr (IVarExpr var)            = liftM IVarExpr (getAddr var)
assignIExpr (IIntExpr num)            = return $ IIntExpr num
assignIExpr (IAopExpr op var1 var2)   = liftM2 (IAopExpr op) (getAddr var1) (getAddr var2)
assignIExpr (IRelopExpr op var1 var2) = liftM2 (IRelopExpr op) (getAddr var1) (getAddr var2)
assignIExpr (IAddrExpr var)           = liftM IAddrExpr (getAddr var)
