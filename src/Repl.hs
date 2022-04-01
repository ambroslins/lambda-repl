module Repl where

import Control.Exception (SomeException (..), try)
import Control.Monad (when)
import Data.Function (on)
import Data.List (nubBy)
import Expr (Env, eval, extend, pretty)
import Parser (Input (..), define, input, parse)
import System.IO (hFlush, stdout)

repl :: IO ()
repl =
  let loop env = do
        putStr "> "
        hFlush stdout
        line <- getLine
        case parse input line of
          Nothing -> do
            putStrLn "syntax error"
            loop env
          Just Empty -> loop env
          Just Quit -> pure ()
          Just (Save path) -> do
            res <- try $ save path env
            putStrLn $ case res of
              Left (SomeException e) -> show e
              Right () -> "saved current environment to " ++ path
            loop env
          Just (Load path) -> do
            res <- try $ load path
            let (newEnv, output) = case res of
                  Left (SomeException e) -> (env, show e)
                  Right loadedEnv ->
                    ( loadedEnv ++ env,
                      "loaded " ++ show (length loadedEnv) ++ " definitions environment from " ++ path
                    )
            putStrLn output
            loop newEnv
          Just (Expr expr) -> do
            putStrLn (pretty $ eval env expr)
            loop env
          Just (Define name expr) -> do
            loop $ extend env name (eval env expr)
   in loop []

save :: FilePath -> Env -> IO ()
save path env =
  let definitions = map (\(name, expr) -> name ++ " = " ++ pretty expr) env
   in writeFile path (unlines $ reverse definitions)

load :: FilePath -> IO Env
load path = do
  content <- readFile path
  pure $ foldl go [] (lines content)
  where
    go env line = case parse define line of
      Nothing -> env
      Just (Define name expr) -> extend env name expr
