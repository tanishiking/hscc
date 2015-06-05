module Main where 

import System.Environment
import Text.Parsec

import AST
import Parser


run :: String -> String
run input = case parse parseProgram "smallC" input of
  Left  err -> "No match" ++ show err
  Right val -> show val


main = do 
  args <- getArgs
  file <- readFile $ head args
  putStrLn $ run file
