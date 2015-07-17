module Semantic where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State.Strict
import Data.List
import Text.Parsec

import AST
import CheckedAST
import Environment
import TypeCheck


semanticCheck :: Program -> (CheckedProgram, [String])
semanticCheck prog = runEnv body initialEnv
  where body = do collectGlobalDecl prog
                  ret <- checkProgram prog 
                  case typeCheck ret of
                    (Left errMsg) -> error errMsg
                    (Right _)     -> return ret


checkProgram :: Program -> StateEnv CheckedProgram
checkProgram prog = liftM concat $ mapM checkExternalDecl prog


checkExternalDecl :: ExternalDeclaration -> StateEnv [CheckedEDecl]
checkExternalDecl (Decl pos declarators)            = checkExDeclarators pos declarators
checkExternalDecl (FuncProt _ _ _ _)                = return []
checkExternalDecl (FuncDef pos ty name params stmt) = checkExFuncDef     pos ty name params stmt


checkExDeclarators :: SourcePos -> DeclaratorList -> StateEnv [CheckedEDecl]
checkExDeclarators pos declarators = return [CheckedDecl pos info]
  where
    info = map (makeVarInfo pos globalLevel) declarators

{-
checkExFuncProt :: SourcePos -> Type -> Identifier -> [(Type, Identifier)] -> StateEnv CheckedEDecl
checkExFuncProt pos ty name params =
  return $ CheckedFuncProt pos fInfo paramsInfo
    where
      paramsInfo = map (makeParamInfo pos ) params
      paramsType = map (getType . snd) paramsInfo
      fInfo      = (name, (FProt, ChFunc (convType ty) paramsType, globalLevel))
-}

checkExFuncDef :: SourcePos -> Type -> Identifier -> [(Type, Identifier)] -> Stmt -> StateEnv [CheckedEDecl]
checkExFuncDef pos ty name params body =
  let paramsInfo = map (makeParamInfo pos) params
      paramsType = map (getType . snd) paramsInfo
      cbody      = checkStmt paramLevel body
      fInfo      =  (name, (Func, ChFunc (convType ty) paramsType, globalLevel)) in do
  levCheckedBody <- withNewEnv paramLevel
                               (mapM_ (appendWithDupCheck pos paramLevel) paramsInfo)
                               cbody
  return [CheckedFuncDef pos fInfo paramsInfo levCheckedBody]


checkStmt :: Level -> Stmt -> StateEnv CheckedStmt
checkStmt _ (EmptyStmt _)    = return CheckedEmptyStmt
checkStmt lev (ExprStmt _ e) = do
  cexpr <- checkExpr lev e
  return $ CheckedExprStmt cexpr
checkStmt lev (CompoundStmt pos dList stmts) =
  let declsInfo = map (makeVarInfo pos (lev+1)) dList
      cstmts    = mapM (checkStmt (lev+1)) stmts in do
  levCheckedStmts <- withNewEnv (lev+1)
                                (mapM_ (appendWithDupCheck pos (lev+1)) declsInfo)
                                cstmts
  return $ CheckedCompoundStmt declsInfo levCheckedStmts
checkStmt lev (IfStmt pos e s1 s2) = do
  ce  <- checkExpr lev e
  cs1 <- checkStmt lev s1
  cs2 <- checkStmt lev s2
  return $ CheckedIfStmt pos ce cs1 cs2
checkStmt lev (WhileStmt pos e body) = do
  ce    <- checkExpr lev e
  cbody <- checkStmt lev body
  return $ CheckedWhileStmt pos ce cbody
checkStmt lev (ReturnStmt pos e) = do
  ce <- checkExpr lev e
  return $ CheckedReturnStmt pos ce


checkExpr :: Level -> Expr -> StateEnv CheckedExpr
checkExpr lev (AssignExpr pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedAssignExpr pos cexpr1 cexpr2
checkExpr lev (Or pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedOr pos cexpr1 cexpr2
checkExpr lev (And pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedAnd pos cexpr1 cexpr2
checkExpr lev (Equal pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedEqual pos cexpr1 cexpr2
checkExpr lev (NotEqual pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedNotEqual pos cexpr1 cexpr2
checkExpr lev (Lt pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedLt pos cexpr1 cexpr2
checkExpr lev (Gt pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedGt pos cexpr1 cexpr2
checkExpr lev (Lte pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedLte pos cexpr1 cexpr2
checkExpr lev (Gte pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedGte pos cexpr1 cexpr2
checkExpr lev (Plus pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedPlus pos cexpr1 cexpr2
checkExpr lev (Minus pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedMinus pos cexpr1 cexpr2
checkExpr lev (Multiple pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedMultiple pos cexpr1 cexpr2
checkExpr lev (Divide pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedDivide pos cexpr1 cexpr2
checkExpr lev (UnaryAddress pos e) = do
  cexpr <- checkExpr lev e
  return $ CheckedUnaryAddress pos cexpr
checkExpr lev (UnaryPointer pos e) = do
  cexpr <- checkExpr lev e
  return $ CheckedUnaryPointer pos cexpr
checkExpr lev (CallFunc pos name params) = do
  funcInfo <- findOrErr pos lev name
  cparams    <- mapM (checkExpr lev) params
  return $ CheckedCallFunc pos funcInfo cparams
checkExpr lev (ExprList pos exprs) = do
  cexprs   <- mapM (checkExpr lev) exprs
  return $ CheckedExprList pos cexprs
checkExpr _ (Constant pos num) = return $ CheckedConstant pos num
checkExpr lev (IdentExpr pos name) = do
  info <- findOrErr pos lev name
  case info of
    (_, (Func, _, _))  -> error $ concat [show pos, "invalid reference to function: ", show name]
    (_, (FProt, _, _)) -> error $ concat [show pos, "invalid reference to function prototype: ", show name]
    varInfo            -> return $ CheckedIdentExpr pos varInfo


