module Environment where

import qualified Data.Map as M
import Text.Parsec
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State.Strict
import AST

type StateEnv = StateT Env (Writer [String])

type Level = Integer

type Env = M.Map Level [Info]
type Info = (Identifer, (Kind, ChType, Level))
data Kind = Var | Func | FuncProt | Param
          deriving (Show, Eq, Ord)

data ChType = ChInt
           | ChVoid
           | ChName
           | ChPointer ChType
           | ChArray   ChType Integer
           | ChFunc    ChType [ChType]
           deriving (Show, Eq, Ord)


appendEnv :: Level -> Info -> StateEnv ()
appendEnv level info = liftM (M.insertWith (++) level [info]) get >>= put


collectGlobalDecl :: Program -> StateEnv ()
collectGlobalDecl = mapM_ collectExternalDecl


collectExternalDecl :: ExternalDeclaration -> StateEnv ()
collectExternalDecl (Decl pos declarators) =  mapM_ (appendEnv 0) (map (convVar pos 0) declarators)
collectExternalDecl (FuncProt pos (FunctionPrototype fpos ty name args)) = do
  let funcInfo = funcDecl ty args FuncProt in
  info <- findAtTheLevel 0 name
  case info of
    (Just i) -> if i == funcInfo
                then return ()
                else error $ concat [show pos, ": invalid prototype decl, ", name]
    Nothing  -> appendEnv 0 (name, funcInfo)
collectExternalDecl (FuncDef pos (FuntionDefinition fpos ty name args stmt)) = do
  let funcInfo = funcDecl ty args Func in
  info <- findAtTheLevel 0 name
  case info of
    (Just i) -> case i of
                  (FuncProt, ty, _) -> if ty == snd' funcInfo
                                       then appendEnv 0 (name, funcInfo)
                                       else error $ concat [show pos, ": invalid decl,", name]
                  -- 同レベルで同じ名前の関数が定義されてたらエラー
                  (Func, _, _)      -> error $ concat [show pos, ": invalid decl,", name]
                  _                 -> appendEnv 0 (name, funcInfo)
    Nothing  -> appendEnv 0 (name, funcInfo)
  

convVar :: SourcePos -> Level -> (Type, DirectDeclarator) -> Info
convVar pos level (t, d) = let ty = convType t in
                           if containVoid ty
                           then error $ concat [show pos, " : variable void "]
                           else case d of
                                (Variable _ name)      -> (name, (Var, ty, level))
                                (Sequence _ name size) -> (name, (Var, CArray ty size, level))


containVoid :: ChType -> Bool
containVoid (ChVoid)       = True
containVoid (ChArray ty _) = containVoid ty
containVoid (ChPointer ty) = containVoid ty
containVoid _              = False 


convType :: Type -> ChType
convType (CInt)        = ChInt
convType (CVoid)       = ChVoid
convType (CPointer ty) = ChPointer $ convType ty


findAtTheLevel :: Level -> Identifer -> StateEnv (Maybe (Kind, ChType, Level))
findAtTheLevel level name = liftM (\env -> M.lookup lev e >>= lookup name) get




fst' :: (a, b, c) -> a
fst' (a, b, c) = a

snd' :: (a, b, c) -> b
snd' (a, b, c) = b

trd' (a, b, c) -> c
trd' (a, b, c) = c
