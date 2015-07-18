module Main where 

import System.Environment
import Text.Parsec
import Control.Exception

import AST
import Parser
import ProgGenerator
import Semantic
import GenIntermed
import AssignAddr
import GenCode


run :: String -> String
run input = 
  let prog = parseProgram input in
  case prog of
    Left  err -> err
    Right val -> (genCode $ assignAddr $ intermed)
--               (concat . snd $ sval)
                   where sval = semanticCheck val
                         intermed = intermedProgram $ fst sval


main :: IO ()
main = do 
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ run file
