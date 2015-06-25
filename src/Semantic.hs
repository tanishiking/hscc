module Semantic (semanticCheckProgram) where

import Control.Monad
import Data.List
import Text.Parsec
import AST

semanticCheckProgram :: Program -> Either String Program
semanticCheckProgram = checkProgram


checkProgram :: Program -> Either String Program
checkProgram = mapM checkExternalDecl


{-==========================
 -   ExternalDeclaration
 ===========================-}

checkExternalDecl :: ExternalDeclaration -> Either String ExternalDeclaration
checkExternalDecl (Decl pos declarators)  = checkExDeclarators pos declarators
checkExternalDecl (FuncProt pos funcProt) = checkExFuncProt    pos funcProt
checkExternalDecl (FuncDef pos funcDef)   = checkExFuncDef     pos funcDef


checkExDeclarators :: SourcePos -> DeclaratorList -> Either String ExternalDeclaration
checkExDeclarators pos declarators = liftM2 Decl cpos cdeclarators
  where
    cpos         = checkPosition pos
    cdeclarators = checkDeclaratorList declarators


checkExFuncProt :: SourcePos -> FunctionPrototype -> Either String ExternalDeclaration
checkExFuncProt pos funcProt = liftM2 FuncProt cpos cfuncProt
  where
    cpos      = checkPosition pos
    cfuncProt = checkFuncProt funcProt


checkExFuncDef :: SourcePos -> FunctionDefinition -> Either String ExternalDeclaration
checkExFuncDef pos funcDef = liftM2 FuncDef cpos cfuncDef
  where
    cpos     = checkPosition pos
    cfuncDef = checkFuncDef funcDef


checkDeclaratorList :: DeclaratorList -> Either String DeclaratorList
checkDeclaratorList = mapM checkDeclarator


checkDeclarator :: (Type, DirectDeclarator) -> Either String (Type, DirectDeclarator)
checkDeclarator = return

{- =====================
 -  Function Prototype
 - =====================-}
 
checkFuncProt :: FunctionPrototype -> Either String FunctionPrototype
checkFuncProt (FunctionPrototype pos ty name params) = do
  newCheckFuncProt <- liftM4 FunctionPrototype cpos cty cname cparams 
  return newCheckFuncProt
    where
      cpos    = checkPosition pos
      cty     = checkType ty
      cname   = checkIdentifier name
      cparams = checkParams params
      

checkFuncDef :: FunctionDefinition -> Either String FunctionDefinition
checkFuncDef (FunctionDefinition pos ty name params stmt) = do
  newCheckFuncDef <- liftM5 FunctionDefinition cpos cty cname cparams cstmt
  return newCheckFuncDef
    where
      cpos    = checkPosition pos
      cty     = checkType ty
      cname   = checkIdentifier name
      cparams = checkParams params
      cstmt   = checkStmt stmt


checkPosition :: SourcePos -> Either String SourcePos
checkPosition = return


checkType :: Type -> Either String Type
checkType = return

checkStmt :: Stmt -> Either String Stmt
checkStmt = return

checkParams :: [(Type, Identifier)] -> Either String [(Type, Identifier)]
checkParams params = return params
  
checkIdentifier :: Identifier -> Either String Identifier
checkIdentifier = return
