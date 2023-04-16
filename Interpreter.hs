module Main where

import Prelude ( IO, getContents )

import TypeCheck ( typeCheck )

main :: IO ()
main = do
  content <- getContents
  case typeCheck content of
    Left err -> putStrLn err
    Right () -> putStrLn "OK"