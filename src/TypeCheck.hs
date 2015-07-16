module TypeCheck where

import Text.Parsec
import Control.Monad
import Debug.Trace

import AST
import CheckedAST
import Environment

typeCheck :: CheckedProgram -> Either String ()
typeCheck = mapM_ eDeclTypeCheck


getRetFuncType :: (Kind, ChType, Level) -> Maybe ChType
getRetFuncType (k, t, l) =
  case t of
    (ChFunc ct _) -> Just ct 
    _             -> Nothing


eDeclTypeCheck :: CheckedEDecl -> Either String ()
eDeclTypeCheck (CheckedDecl _ _) = return ()
--eDeclTypeCheck (CheckedFuncProt pos info args) = return ()
eDeclTypeCheck (CheckedFuncDef pos (fname, finfo) args stmt) =
  let maybeExpectedRetType = getRetFuncType finfo in do
    case maybeExpectedRetType of
      (Just expType) -> let eitherType = stmtTypeCheck (fname, finfo) stmt in do
                        case eitherType of
                          (Left err)      -> fail err
                          (Right actType) -> 
                            if (expType == actType) 
                              then return () 
                              else fail $ concat [show pos, " invalid type error: \n  Expected:", show expType, "\n  Actual:", show actType]
      Nothing   -> fail $ concat [show pos, " invalid function return type", show (getType finfo)]


stmtTypeCheck :: Info -> CheckedStmt -> Either String ChType
stmtTypeCheck _ (CheckedEmptyStmt) = return ChVoid
stmtTypeCheck _ (CheckedExprStmt e) = exprTypeCheck e >> return ChVoid
stmtTypeCheck info (CheckedCompoundStmt _ stmts) = do
  stmtsType <- mapM (stmtTypeCheck info) stmts
  return $ maximum stmtsType
stmtTypeCheck info (CheckedIfStmt pos cond true false) =
  let eitherExprTy = exprTypeCheck cond in
  case eitherExprTy of
    (Left err)     -> fail err
    (Right expTy) -> if expTy /= ChInt
                     then fail $ concat [show pos, "invalid expression", show cond, " - it must be int"]
                     else liftM2 max trueTy falseTy
                     where trueTy  = stmtTypeCheck info true 
                           falseTy = stmtTypeCheck info false
stmtTypeCheck info (CheckedWhileStmt pos cond stmt) =
  let eitherExprTy = exprTypeCheck cond in
  case eitherExprTy of
  (Left err)    -> fail err
  (Right expTy) -> if expTy /= ChInt
                     then fail $ concat [show pos, "invalid expression", show cond, " - it must be int"]
                     else stmtTypeCheck info stmt 
stmtTypeCheck (fname, finfo) (CheckedReturnStmt pos e) =
  let maybeExpectedRetType = getRetFuncType finfo in do
    case maybeExpectedRetType of
      (Just expType) -> do actualType <- exprTypeCheck e
                           if expType == actualType
                             then return actualType
                             else fail $ concat [show pos, "type mismatch \n  Expected: ", show expType, "\n  Actual: ", show actualType]
      Nothing   -> fail $ concat [show pos, " invalid function return type", show (getType finfo)]


exprTypeCheck :: CheckedExpr -> Either String ChType
exprTypeCheck (CheckedAssignExpr pos e1 e2) = do
  checkAssignForm pos e1
  ty1 <- exprTypeCheck e1
  ty2 <- exprTypeCheck e2
  if ty1 == ty2 
    then return ty1
    else fail $ concat [show pos, "type mismatch", show ty1, " and ", show ty2]
exprTypeCheck (CheckedOr pos e1 e2)       = checkBothInt pos e1 e2
exprTypeCheck (CheckedAnd pos e1 e2)      = checkBothInt pos e1 e2
exprTypeCheck (CheckedEqual pos e1 e2)    = checkCompare pos e1 e2
exprTypeCheck (CheckedNotEqual pos e1 e2) = checkCompare pos e1 e2
exprTypeCheck (CheckedLt pos e1 e2)       = checkCompare pos e1 e2
exprTypeCheck (CheckedGt pos e1 e2)       = checkCompare pos e1 e2
exprTypeCheck (CheckedLte pos e1 e2)      = checkCompare pos e1 e2
exprTypeCheck (CheckedGte pos e1 e2)      = checkCompare pos e1 e2
exprTypeCheck (CheckedPlus pos e1 e2)     = checkAddSub pos e1 e2
exprTypeCheck (CheckedMinus pos e1 e2)    = checkAddSub pos e1 e2
exprTypeCheck (CheckedMultiple pos e1 e2) = checkBothInt pos e1 e2
exprTypeCheck (CheckedDivide pos e1 e2)   = checkBothInt pos e1 e2
exprTypeCheck (CheckedUnaryAddress pos e) = do
  checkAddressRefer pos e
  ty <- exprTypeCheck e
  if ty == ChInt
    then return $ ChPointer ChInt
    else fail $ concat [show pos, "invalid operand &: it must be use for int type"]
exprTypeCheck (CheckedUnaryPointer pos e) = do
  ty <- exprTypeCheck e
  case ty of
    (ChPointer pty) -> return pty
    ChInt           -> return ChInt
    _               -> fail $ concat [show pos, "invalid pointer refer, you cannot refer ", show ty]
exprTypeCheck (CheckedCallFunc pos funcInfo args) = do
  argTypes <- mapM exprTypeCheck args
  case (snd $ funcInfo) of
    (Func, ChFunc ty paramTypes, _) -> if argTypes == paramTypes
                                          then return ty
                                          else fail $ concat [show pos, "type mismatch in func params\n  Expected: ", show paramTypes, "\n  Actual: ", show argTypes]
exprTypeCheck (CheckedExprList pos exprs)         = liftM last (mapM exprTypeCheck exprs)
exprTypeCheck (CheckedConstant pos num)           = return ChInt
exprTypeCheck (CheckedIdentExpr pos (name, info)) = return $ getType info



checkBothInt :: SourcePos -> CheckedExpr -> CheckedExpr -> Either String ChType
checkBothInt pos e1 e2 = do
  ty1 <- exprTypeCheck e1
  ty2 <- exprTypeCheck e2
  if ty1 == ChInt && ty2 == ChInt
    then return ChInt
    else fail $ concat [show pos, " type mismatch: both ", show e1, " and ", show e2, " must be int"]


checkCompare :: SourcePos -> CheckedExpr -> CheckedExpr -> Either String ChType
checkCompare pos e1 e2 = do
  ty1 <- exprTypeCheck e1
  ty2 <- exprTypeCheck e2
  if ty1 == ty2
    then return ChInt
    else fail $ concat [show pos, " type mismatch: \n", show e1, ": ", show ty1, "\n", show e2, ": ", show ty2]


checkAddSub :: SourcePos -> CheckedExpr -> CheckedExpr -> Either String ChType
checkAddSub pos e1 e2 = do
  ty1 <- exprTypeCheck e1
  ty2 <- exprTypeCheck e2
  case (ty1, ty2) of
    (ChInt, ChInt)        -> return ChInt
    (ChPointer ty, ChInt) -> return $ ChPointer ty
    --(ChInt, ChPointer Chint) -> return $ ChPointer ChInt
    (ChArray ty _, ChInt) -> return $ ChPointer ty
    _                     -> fail $ concat [show pos, " type mismatch: \n", show e1, ": ", show ty1, "\n", show e2, ": ", show ty2]


{- 式の形の検査 -}
checkAssignForm :: SourcePos -> CheckedExpr -> Either String ()
checkAssignForm pos (CheckedIdentExpr _ (name, (kind, ty, _))) =
  case (kind, ty) of
    (Var, (ChArray _ _ ))   -> fail $ concat [show pos, "invalid assignment to: ", show name] 
    (Var, _)                -> return ()
    (Func, _)               -> fail $ concat [show pos, "invalid assignment to: ", show name] 
    (FProt, _)              -> fail $ concat [show pos, "invalid assignment to: ", show name] 
    (Param, (ChArray _ _ )) -> fail $ concat [show pos, "invalid assignment to: ", show name] 
    (Param, _)              -> return ()
checkAssignForm pos (CheckedUnaryPointer _ e) = return ()
checkAssignForm pos _ = fail $ concat [show pos, " invalid assign form"]


checkAddressRefer :: SourcePos -> CheckedExpr -> Either String ()
checkAddressRefer pos (CheckedIdentExpr _ _) = return ()
checkAddressRefer pos _ = fail $ concat [show pos, "invalid operand &: it must be used for Identifier"]
