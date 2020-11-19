module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = getArgs >>= print . prefix . unwords

prefix s = "Args:=> " ++ s
