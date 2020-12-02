{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.HUnit
import Test.Tasty
import Test.Hspec
import Test.DocTest

import Ads_b

import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shift, xor)

prop_reverseReverse :: [Int] -> Bool
prop_reverseReverse xs = reverse (reverse xs) == xs

prop_Crc24 :: [Word8] -> Bool
prop_Crc24 xs = crc24 (x1:x2:x3:xs) == 0 where
    crc' = crc24DataOnly xs
    x1 = fromIntegral (crc' Data.Bits..&. 0xFF)
    x2 = fromIntegral ((crc' `shift` (-8)) Data.Bits..&. 0xFF)
    x3 = fromIntegral ((crc' `shift` (-16)) Data.Bits..&. 0xFF)


{-UF/DF 4:    0x20 00 00 00 00 00 00,                         CRC-24 = 80665F-}
{-UF/DF 5:    0x28 00 00 00 00 00 00,                         CRC-24 = 2078CE-}
{-UF/DF 20:   0xA0 00 00 00 00 00 00 00 00 00 00 00 00 00,    CRC-24 = C88294-}
{-UF/DF 21:   0xA8 00 00 00 00 00 00 00 00 00 00 00 00 00,    CRC-24 = 0B154F-}

testUf2 :: [Word8]
testUf2 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
addr2, crc2 :: Word32
addr2 = 0x003FABF2
crc2 = 0x80665F
res2 = 0xAAAAAA


testUf3 :: [Word8]
testUf3 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0]
addr3, crc3 :: Word32
addr3 = 0xACC555
crc3 = 0xC88294
res3 = 0

test1 = TestCase (assertEqual "for crc24 (1):" (crc2) (crc24 testUf2))
test2 = TestCase (assertEqual "for crc24 (2):" (crc3) (crc24 testUf3))
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]


return []
{-main :: IO ()-}
{-main = putStrLn "Test suite not yet implemented"-}
main = do
    $(quickCheckAll)
    runTestTT tests
{-main = quickCheck prop_Crc24-}
