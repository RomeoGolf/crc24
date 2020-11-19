module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    print . prefix . unwords $ args
    let fname = head args
    print fname

prefix s = "Args:=> " ++ s
