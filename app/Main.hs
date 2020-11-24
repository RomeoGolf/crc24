module Main where

import System.Environment (getArgs)
import System.IO
import Numeric (readHex)
import Data.Word
import Data.Bits
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

maskC :: Word32
maskC = 0x80000000

poly :: Word32
poly = 0xFFF40900

preparedData :: [Word8] -> (Word32, [Word8])
preparedData (x0:x1:x2:xs) = let
    initBuf = (fromIntegral x0) `shift` 24
                .|. (fromIntegral x1) `shift` 16
                .|. (fromIntegral x2) `shift` 08
    in (initBuf, xs)
preparedData _ = error "The data too short!"

