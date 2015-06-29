module Main where 

import System.Environment
import Text.Parsec
import Control.Exception

import AST
import Parser
import ProgGenerator
import Semantic


run :: String -> String
run input = 
  let prog = parseProgram input in
  case prog of
    Left  err -> err
    Right val -> (show . fst $ sval) ++ "\n" ++ (concat . snd $ sval)
                   where sval = semanticCheck val


main :: IO ()
main = do 
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ run file
