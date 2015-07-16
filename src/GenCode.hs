module GenCode where

import Intermed
import AssignAddr

import Data.List
import Control.Monad.State

type Asm = String

type LabelEnv = State (Int, Int)


runLabelEnv :: LabelEnv a -> a
runLabelEnv s = evalState s (0, 0)


withLineBreaks :: [String] -> String
withLineBreaks = concat . intersperse "\n"


getFuncFrameSize :: IStmt -> Int
getFuncFrameSize (ICompoundStmt decls stmts)
  = minimum $ (map getFpAddr decls) ++ (map getFuncFrameSize stmts)
getFuncFrameSize _ = 0


setStackPointer :: Int -> LabelEnv ()
setStackPointer size = do
  (n, m) <- get
  put (n, size)


prettyInst :: [String] -> String
prettyInst (op:args) = concat ["    ", op, " ", concat (intersperse ", " args)]


genCode :: IProgram -> Asm
genCode prog = header ++ body
  where header = "    .text" ++ "\n" ++   ".globl main" ++  "\n"
        body   = runLabelEnv (do asm <- mapM genIExDecl prog
                                 return $ withLineBreaks asm)


t :: Int -> String
t n = "$t" ++ show n


genIExDecl :: IExDecl -> LabelEnv Asm
genIExDecl (IFuncDef (VarInfo func) args body) = do
  let fpSize = (length args) * wordSize
      spSize = abs $ getFuncFrameSize body
  setStackPointer (-spSize)
  stmt <- genStmt body
  return (withLineBreaks $ 
            [(fst func) ++ ":"
         -- $spを後にfpの基軸とするために退避しておく
            ,prettyInst ["move", "$t0", "$sp"]
         -- スタックにスペースを確保
            ,prettyInst ["addi","$sp", "$sp", show (-spSize)]
            ,prettyInst ["sw", "$fp", "0($sp)"] -- fpを退避
            ,prettyInst ["sw", "$ra", "4($sp)"] -- raを退避
         -- fpを移動し引数のためのスペースを確保
            ,prettyInst ["addi", "$fp", "$t0", show (-fpSize)]]
         ++ stmt
         -- 呼び出し元に制御を戻す
         ++ [prettyInst ["lw", "$ra", "4($sp)"] -- raを復元
            ,prettyInst ["lw", "$fp", "0($sp)"] -- fpを復元
            ,prettyInst ["move", "$sp", "$t0"] -- spを復元
            ,prettyInst ["jr", "$ra"]] -- 制御を呼び出し元に戻す
         )
genIExDecl _ = return ""


genStmt :: IStmt -> LabelEnv [Asm]
genStmt (IEmptyStmt) = return [""]
genStmt (ILetStmt dest srcExpr) = do
  expr <- genExpr srcExpr
  return $ expr ++ [prettyInst ["sw", "$t0", show dest]]
genStmt (IWriteStmt dest src) =
  return $ [prettyInst ["sw", show src, "0(" ++ show dest ++ ")"]]
genStmt (IReadStmt dest src) =
  return $ [prettyInst ["lw", "0(" ++ show src ++ ")", show dest]]
genStmt (IReturnStmt retvar) =
  return $ [prettyInst ["li", "$v0", show 1]
           ,prettyInst ["li", "$a0", show retvar]
           ,"syscall"]
genStmt (ICompoundStmt decls stmts) = liftM concat (mapM genStmt stmts)

genExpr :: IExpr -> LabelEnv [Asm]
genExpr (IVarExpr var) =
  return $ [prettyInst ["lw", "$t0", show var]]
genExpr (IIntExpr num) =
  return $ [prettyInst ["li", "$t0", show num]]
genExpr (IAopExpr op v1 v2) = 
  return $ [prettyInst ["lw", "$t1", show v1]
           ,prettyInst ["lw", "$t2", show v2]] ++ genAop op
genExpr (IAddrExpr (VarAddr addr)) =
  case addr of
    (Fp n) -> return $ [prettyInst ["addi", "$t0", "$fp", show n]]
    (Gp n) -> return $ [prettyInst ["addi", "$t0", "$gp", show n]]


genAop :: String -> [String]
genAop "+" = [prettyInst ["add",  "$t0", "$t1", "$t2"]]
genAop "-" = [prettyInst ["sub",  "$t0", "$t1", "$t2"]]
genAop "*" = [prettyInst ["mult", "$t0", "$t1", "$t2"]]
genAop "/" = [prettyInst ["div",  "$t0", "$t1", "$t2"]]
