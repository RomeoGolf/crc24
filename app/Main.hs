module Main where

import System.Environment (getArgs)
import System.IO
import Lib

main :: IO ()
main = do
    args <- getArgs
    print . prefix . unwords $ args
    let fname = head args
    print fname
    content <- readFile fname
    print $ map readInt (words content)

prefix s = "Args:=> " ++ s

readInt :: String -> Int
readInt = read
