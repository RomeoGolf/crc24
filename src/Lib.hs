module Lib
    ( someFunc
    ) where

import System.Environment (getArgs)

someFunc :: IO ()
someFunc = getArgs >>= print . prefix . head
{-someFunc = putStrLn "Yo!"-}

prefix s = "Args:> " ++ s
