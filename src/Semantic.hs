module Semantic where

import Control.Monad.State.Strict
import Control.Monad.Writer
import Control.Monad
import Data.List
import Text.Parsec

import AST
import CheckedAST
import Environment


semanticCheck :: Program -> (CheckedProgram, [String])
semanticCheck prog = runEnv body initialEnv
  where body = do collectGlobalDecl prog
                  ret <- checkProgram prog 
                  return ret


checkProgram :: Program -> StateEnv CheckedProgram
checkProgram = mapM checkExternalDecl


{-==========================
 -   ExternalDeclaration
 ===========================-}

checkExternalDecl :: ExternalDeclaration -> StateEnv CheckedEDecl 
checkExternalDecl (Decl pos declarators)          = checkExDeclarators pos declarators
checkExternalDecl (FuncProt pos ty name params)     = checkExFuncProt    pos ty name params
checkExternalDecl (FuncDef pos ty name params stmt) = checkExFuncDef     pos ty name params stmt


checkExDeclarators :: SourcePos -> DeclaratorList -> StateEnv CheckedEDecl
checkExDeclarators pos declarators = return $ CheckedDecl pos info 
  where
    info = map (makeVarInfo pos globalLevel) declarators


checkExFuncProt :: SourcePos -> Type -> Identifier -> [(Type, Identifier)] -> StateEnv CheckedEDecl
checkExFuncProt pos ty name params =
  return $ CheckedFuncProt pos fInfo paramsinfo
    where
      fInfo    = (name, (FProt, convType ty, globalLevel))
      paramsinfo = map (makeParamInfo pos ) params


checkExFuncDef :: SourcePos -> Type -> Identifier -> [(Type, Identifier)] -> Stmt -> StateEnv CheckedEDecl
checkExFuncDef pos ty name params body =
  let fInfo      =  (name, (FProt, convType ty, globalLevel))
      paramsInfo = map (makeParamInfo pos) params in do
  cbody    <- checkStmt 2 body
  return $ CheckedFuncDef pos fInfo paramsInfo cbody


checkStmt :: Level -> Stmt -> StateEnv CheckedStmt
checkStmt lev (EmptyStmt _)  = return CheckedEmptyStmt
checkStmt lev (ExprStmt _ e) = do
  cexpr <- checkExpr lev e
  return $ CheckedExprStmt cexpr
checkStmt lev (CompoundStmt pos dLists stmts) =
  let declsInfo = map (map (makeVarInfo pos lev)) dLists in do
  cstmts <- mapM (checkStmt lev) stmts
  return $ CheckedCompoundStmt declsInfo cstmts
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
checkExpr lev (Devide pos e1 e2) = do
  cexpr1 <- checkExpr lev e1
  cexpr2 <- checkExpr lev e2
  return $ CheckedDevide pos cexpr1 cexpr2
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
checkExpr lev (Constant pos num) = return $ CheckedConstant pos num
checkExpr lev (IdentExpr pos name) = do
  info <- findOrErr pos lev name
  case info of
    (_, (Func, _, _))     -> error $ concat [show pos, "invalid reference to function: ", show name]
    (_, (FProt, _, _)) -> error $ concat [show pos, "invalid reference to function prototype: ", show name]
    varInfo               -> return $ CheckedIdentExpr pos varInfo


