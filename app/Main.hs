module Main where

import System.Environment (getArgs)
import System.IO
import Numeric (readHex)
import Lib

main :: IO ()
main = do
    args <- getArgs
    let fname = head args
    print fname
    content <- readFile fname
    print $ intListFromHex content

intListFromHex :: String -> [Int]
intListFromHex hexStr = map (fst . head . readHex) (words hexStr)
