module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    print . prefix . unwords $ args

prefix s = "Args:=> " ++ s
