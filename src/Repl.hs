module Repl where

import Control.Monad (when)
import Expr (Input (..), eval, extend, pretty)
import Parser (input, parse)
import System.IO (hFlush, stdout)

repl :: IO ()
repl =
  let loop env = do
        putStr "> "
        hFlush stdout
        line <- getLine
        case parse input line of
          Nothing -> putStrLn "syntax error"
          Just Empty -> loop env
          Just Quit -> pure ()
          Just (Expr expr) -> do
            putStrLn (pretty $ eval env expr)
            loop env
          Just (Define name expr) -> do
            loop $ extend env name (eval env expr)
   in loop []