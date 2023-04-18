module Main where

import Prelude ( IO, getContents, ($), String, Show )
import Data.Either

import ParGramatyka ( pProgram, myLexer )
import LexGramatyka ( Token )
import PrintGramatyka ( Print )

import TypeCheck ( typeCheck )
import System.Exit (exitFailure)
import System.IO (putStrLn)
import AbsGramatyka (Program)

type Err      = Either String
type ParseFun = [Token] -> Err Program

runTypeCheck :: ParseFun -> String -> IO ()
runTypeCheck p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      case typeCheck tree of
        Left err -> do
          putStrLn "\nTypeCheck          Failed...\n"
          putStrLn err
          exitFailure
        Right _ -> do
          putStrLn "\nTypeCheck          Successful!"
  where
  ts = myLexer s

main :: IO ()
main = do
  content <- getContents
  runTypeCheck pProgram content