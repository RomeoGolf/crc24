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
preparedData _ = error "The data is too short!"

crc24' :: (Word32, [Word8]) -> Word32
crc24' (buf, []) = buf `shift` (-8)
crc24' (buf, x:xs) = let
    buf' = buf .|. (fromIntegral x)
    processedBuf :: Word32 -> Int -> Word32
    processedBuf b 0 = b
    processedBuf b cnt = let
        cBit = (b .&. maskC) /= 0
        b' = b `shift` (1)
        b'' = if cBit then b' `xor` poly else b'
        in processedBuf b'' (pred cnt)
    in crc24' (processedBuf buf' 8, xs)

crc24 :: [Word8] -> Word32
crc24 = crc24' . preparedData . reverse

