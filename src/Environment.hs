module Environment where

import qualified Data.Map as M
import Text.Parsec
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State.Strict
import AST
import Debug.Trace

{- 環境とエラーメッセージを持つstate -}
type StateEnv = StateT Env (Writer [String])

type Level = Integer

type Env = M.Map Level [Info]
type Info = (Identifier, (Kind, ChType, Level))
data Kind = Var | Func | FProt | Param
          deriving (Show, Eq)

data ChType = ChVoid
            | ChInt
            | ChPointer ChType
            | ChArray   ChType Integer
            | ChFunc    ChType [ChType]
            | ChTmp
           deriving (Show, Ord)

instance Eq ChType where
  (==) ChVoid             ChVoid             = True
  (==) ChInt              ChInt              = True
  (==) ChTmp              ChTmp              = True
  (==) (ChPointer ty1)    (ChPointer ty2)    = ty1 == ty2
  (==) (ChArray ty1 _)    (ChArray ty2 _)    = ty1 == ty2
  (==) (ChFunc ty1 args1) (ChFunc ty2 args2) = ty1 == ty2 && args1 == args2
  (==) (ChPointer ty1)  (ChArray ty2 _)      = ty1 == ty2
  (==) (ChArray ty1 _)  (ChPointer ty2)      = ty1 == ty2
  (==) _ _                                   = False



globalLevel :: Level
globalLevel = 0

paramLevel :: Level
paramLevel = 1

bodyLevel :: Level
bodyLevel = 2


runEnv :: StateEnv a -> Env -> (a, [String])
runEnv s env = runWriter (evalStateT s env)


initialEnv :: Env
initialEnv = M.fromList [(0, [("print", (Func, chfunc, globalLevel))])]
  where chfunc = ChFunc ChVoid [ChInt]


--まずcollectGlobalDeclでグローバルの情報を収集し環境に保存
collectGlobalDecl :: Program -> StateEnv ()
collectGlobalDecl = mapM_ collectExternalDecl


collectExternalDecl :: ExternalDeclaration -> StateEnv ()
collectExternalDecl (Decl pos declarators) = mapM_ (appendWithDupCheck pos globalLevel) (map (makeVarInfo pos globalLevel) declarators)
collectExternalDecl (FuncProt pos ty name args)
  = let funcInfo = makeFuncInfo ty name args FProt in do
    maybeInfo <- findAtTheLevel globalLevel name
    case maybeInfo of
      (Just info) -> if info == snd funcInfo
                     then return ()  -- プロトタイプ二重宣言
                     else error $ concat [show pos, ": invalid prototype decl, "
                                         ,name, ": ", name, " is already declared"]
      Nothing  -> appendEnv globalLevel funcInfo
collectExternalDecl (FuncDef pos ty name args stmt)
  = let funcInfo = makeFuncInfo ty name args Func in do
    maybeinfo <- findAtTheLevel globalLevel name
    case maybeinfo of
      (Just info) -> case info of
                     (FProt, ty, _) -> if ty == (getType . snd $ funcInfo)
                                          then appendEnv globalLevel funcInfo
                                          else error $ concat [show pos, ": invalid declaration: ", name]
                     (Func, _, _)   -> error $ concat [show pos
                                                      ,": invalid declaration "
                                                      , name, ": function "
                                                      , name, " is already declared"]
                     (Var, _, _)    -> error $ concat [show pos
                                                      ,": invalid declaration "
                                                      ,name , ": variable "
                                                      ,name, "is already declared"]
                     _              -> appendEnv globalLevel funcInfo
      --Nothing  -> error $ concat [show pos, "you cannot call function without prototype declaration: ", name] 
      Nothing -> appendEnv globalLevel funcInfo


{-============================
 -        Utility
 - ===========================-}

appendEnv :: Level -> Info -> StateEnv ()
appendEnv level info = liftM (M.insertWith (++) level [info]) get >>= put


appendWithDupCheck :: SourcePos -> Level -> Info -> StateEnv ()
appendWithDupCheck pos lev info =
  let name = fst info in do
    maybeInfo <- find lev name
    case maybeInfo of
      (Just mInfo) -> if (getLevel . snd $ mInfo) == paramLevel
                      then (tell $ [concat [show pos, "warning: the variable,", show name, "is already declared in parameter"]]) >> appendEnv bodyLevel info
                      --else error $ concat [show pos, "duplicate variable: ", show name]
                      else appendEnv lev info
      Nothing  -> appendEnv lev info


makeVarInfo :: SourcePos -> Level -> (Type, DirectDeclarator) -> Info
makeVarInfo pos level (t, d) 
  = let ty = convType t in
    if containVoid ty
    then error $ concat [show pos, " : variable void "]
    else case d of
      (Variable _ name)      -> (name, (Var, ty, level))
      (Sequence _ name size) -> (name, (Var, ChArray ty size, level))


makeFuncInfo :: Type -> Identifier -> [(Type, Identifier)] -> Kind -> Info
makeFuncInfo ty name args kind
  = (name, (kind, funcTy, globalLevel))
    where returnTy = convType ty
          argsTy   = map (convType . fst) args  
          funcTy   = (ChFunc returnTy argsTy)


makeParamInfo :: SourcePos -> (Type, Identifier) -> Info
makeParamInfo pos (ty, name)
  = let cty = convType ty in
    if containVoid cty 
    then error $ concat [show pos, "invalid argument: ", show name, "it's void type"]
    else (name, (Param, cty, paramLevel))



containVoid :: ChType -> Bool
containVoid (ChVoid)       = True
containVoid (ChArray ty _) = containVoid ty
containVoid (ChPointer ty) = containVoid ty
containVoid _              = False 


convType :: Type -> ChType
convType (CInt)        = ChInt
convType (CVoid)       = ChVoid
convType (CPointer ty) = ChPointer $ convType ty


findAtTheLevel :: Level -> Identifier -> StateEnv (Maybe (Kind, ChType, Level))
findAtTheLevel lev name = liftM (\e -> M.lookup lev e >>= lookup name) get


delLevel :: Level -> StateEnv ()
delLevel lev = (liftM (M.delete lev) get) >>= put



{- Level:       新しい環境のレベル 
 - StateEnv (): 新しい環境への情報の追加
 - StateEnv a : 新しい環境内での実際の動作
 - StateEnv a : 環境を削除 -}
withNewEnv :: Level -> StateEnv () -> StateEnv a -> StateEnv a
withNewEnv lev append body = append >> body <* delLevel lev


find :: Level -> Identifier -> StateEnv (Maybe Info)
find lev name =
  if lev < 0
  then return Nothing
  else do
    env <- get
    case (M.lookup lev env >>= lookup name) of
      Just info -> return $ Just (name, info)
      Nothing   -> find (lev-1) name


findOrErr :: SourcePos -> Level -> Identifier -> StateEnv Info
findOrErr pos lev name = do
  maybeInfo <- find lev name
  case maybeInfo of
    (Just info) -> return info
    Nothing     -> error $ concat [show pos, "undefined variable:", name]

getKind :: (Kind, ChType, Level) -> Kind
getKind (kind, _, _) = kind

getType :: (Kind, ChType, Level) -> ChType
getType (_, chType, _) = chType

getLevel :: (Kind, ChType, Level) -> Level
getLevel (_, _, level) = level
