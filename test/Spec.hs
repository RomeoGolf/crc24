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

prop_Crc24XorOutUplink :: Word32 -> [Word8] -> Bool
prop_Crc24XorOutUplink xorData xs = crc24XorOut xorData' (x1:x2:x3:xs) == 0 where
    xorData' = xorData --    Data.Bits..&. 0xFFFFFF
    crc' = crc24DataOnlyXorOut xorData' xs
    x1 = fromIntegral (crc' Data.Bits..&. 0xFF)
    x2 = fromIntegral ((crc' `shift` (-8)) Data.Bits..&. 0xFF)
    x3 = fromIntegral ((crc' `shift` (-16)) Data.Bits..&. 0xFF)

{-UF/DF 4:    0x20 00 00 00 00 00 00,                         CRC-24 = 80665F-}
{-UF/DF 5:    0x28 00 00 00 00 00 00,                         CRC-24 = 2078CE-}
{-UF/DF 20:   0xA0 00 00 00 00 00 00 00 00 00 00 00 00 00,    CRC-24 = C88294-}
{-UF/DF 21:   0xA8 00 00 00 00 00 00 00 00 00 00 00 00 00,    CRC-24 = 0B154F-}

testData :: [Word8]
testData = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]
check = 0xA05E66

testMsg1, testMsg2 :: [Word8]
testMsg1 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
testMsg2 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0]


crc1, crc2 :: Word32
crc1 = 0x80665F
crc2 = 0xC88294



addr1 = 0xC051F6
addr2 = 0x3FABF2
addr3 = 0xACC555
addr4 = 0x533F51

encAddr1 = 0x80665F
encAddr2 = 0x2ACCF5
encAddr3 = 0xC88294
encAddr4 = 0x62283E

testEncAddr1 = TestCase (assertEqual "encode address (1):" encAddr1 (encodedAddress addr1))
testEncAddr2 = TestCase (assertEqual "encode address (2):" encAddr2 (encodedAddress addr2))
testEncAddr3 = TestCase (assertEqual "encode address (3):" encAddr3 (encodedAddress addr3))
testEncAddr4 = TestCase (assertEqual "encode address (4):" encAddr4 (encodedAddress addr4))

testCrc24 = TestCase (assertEqual "test CRC24 (main):" check (crc24DataOnly testData))




testCrc1 = TestCase (assertEqual "for crc24 (1):" (crc1) (crc24 testMsg1))
testCrc2 = TestCase (assertEqual "for crc24 (2):" (crc2) (crc24 testMsg2))
tests = TestList [TestLabel "crc24 main" testCrc24
        , TestLabel "crc24-1" testCrc1, TestLabel "crc24-2" testCrc2
        , TestLabel "encode addr test 2" testEncAddr2
        , TestLabel "encode addr test 3" testEncAddr3
        , TestLabel "encode addr test 4" testEncAddr4]


return []
{-main :: IO ()-}
{-main = putStrLn "Test suite not yet implemented"-}
main = do
    $(quickCheckAll)
    runTestTT tests
{-main = quickCheck prop_Crc24-}
