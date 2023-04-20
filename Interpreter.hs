module Main where

import AbsGramatyka (Program)
import Data.Either
import LexGramatyka (Token)
import ParGramatyka (myLexer, pProgram)
import PrintGramatyka (Print)
import Run (Output, run)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, putStrLn, stderr)
import TypeCheck (typeCheck)
import Prelude (Foldable (foldl), IO, Monad (return, (>>)), Show, String, concat, getContents, map, mapM_, putStr, ($), (.), reverse)
import Data.List ((++), unlines)

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

main :: IO ()
main = do
  content <- getContents
  program <- runTypeCheck pProgram content
  runRun run program
