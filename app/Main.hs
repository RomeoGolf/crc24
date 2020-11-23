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
    print $ map (fst . head . readHex) (words content)


