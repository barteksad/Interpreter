module Main where

import Prelude ( IO, getContents, ($), String, Show, putStr )
import Data.Either

import ParGramatyka ( pProgram, myLexer )
import LexGramatyka ( Token )
import PrintGramatyka ( Print )

import TypeCheck ( typeCheck )
import System.Exit (exitFailure)
import System.IO (putStrLn, hPutStrLn, stderr)
import AbsGramatyka (Program)

type Err      = Either String
type ParseFun = [Token] -> Err Program

runTypeCheck :: ParseFun -> String -> IO ()
runTypeCheck p s =
  case p ts of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
      case typeCheck tree of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right _ -> putStr ""
  where
  ts = myLexer s

main :: IO ()
main = do
  content <- getContents
  runTypeCheck pProgram content