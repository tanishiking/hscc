module Main where 

import System.Environment
import Text.Parsec

import AST
import Parser
import ProgGenerator
import Semantic


run :: String -> String
run input = case parseProgram input >>= semanticCheckProgram of
  Left  err -> err
  Right val -> show val


main :: IO ()
main = do 
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ run file
