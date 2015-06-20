module Semantic (semanticCheckProgram) where

import Data.List
import Text.Parsec
import AST

semanticCheckProgram :: Either String Program -> Either String Program
semanticCheckProgram prog = case prog of
                            Left  err  -> Left  err
                            Right code -> Right code
