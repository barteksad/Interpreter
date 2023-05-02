{-# LANGUAGE BlockArguments #-}

module Main where

import AbsGramatyka (Program)
import Data.Either
import Data.List (head, unlines)
import LexGramatyka (Token)
import ParGramatyka (myLexer, pProgram)
import PrintGramatyka (Print)
import Run (Output, run)
-- import Prelude (IO, Monad (return, (>>)), String, putStr, reverse, ($))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, putStrLn, readFile, stderr)
import TypeCheck (typeCheck)

type Err = Either String

type ParseFun = [Token] -> Err Program

type RunFn = Program -> Err Output

runTypeCheck :: ParseFun -> String -> IO Program
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
        Right _ -> return tree
  where
    ts = myLexer s

runRun :: RunFn -> Program -> IO ()
runRun r p =
  case r p of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right s -> putStr $ unlines (reverse s) 

getProgramCode :: IO String
getProgramCode =
  do
    args <- getArgs
    readFile $ head args

main :: IO ()
main =
  do
    content <- getProgramCode
    program <- runTypeCheck pProgram content
    runRun run program
