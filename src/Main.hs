module Main where 

import System.Environment
import Text.Parsec

import AST
import Parser
import ProgGenerator
import Semantic


run :: String -> Either String Program
run input = case parse parseProgram "smallC" input of
  Left  err -> Left (show err)
  Right val -> Right val


main = do 
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ output $ semanticCheckProgram $ run file
    where output code = case code of
                        Left  err -> err
                        Right val -> show val
