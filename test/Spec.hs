
import Test.QuickCheck
import Test.Hspec

import Ads_b

import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shift, xor)

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

testData :: [Word8]
testData = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]
check = 0xA05E66

testMsg1, testMsg2, testMsg3, testMsg4 :: [Word8]
testMsg1 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
testMsg2 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0]
testMsg3 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x28]
testMsg4 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA8]

testAddr1, testAddr2, testAddr3, testAddr4 :: Word32
testAddr1 = 0x00C051F6
testAddr2 = 0x003FABF2
testAddr3 = 0x00ACC555
testAddr4 = 0x00533F51

testAddrDf1, testAddrDf2, testAddrDf3, testAddrDf4 :: Word32
testAddrDf1 = 0x002078CE
testAddrDf2 = 0x00752D9B
testAddrDf3 = 0x000b154F
testAddrDf4 = 0x005E401A

crc1, crc2, crc3, crc4 :: Word32
crc1 = 0x80665F
crc2 = 0xC88294
crc3 = 0x2078CE
crc4 = 0x0B154F

ap1 = 0
ap2 = 0xAAAAAA
ap3 = 0x555555

encAddr1 = 0x80665F
encAddr2 = 0x2ACCF5
encAddr3 = 0xC88294
encAddr4 = 0x62283E

main :: IO ()
main = hspec $ do
    describe "ADS-B" $ do
        it "CRC24 calculation & checking" $
            property $ prop_Crc24
        it "CRC24 with XorOut calculation & checking" $
            property $ prop_Crc24XorOutUplink

        it "CRC24 main test" $
            crc24DataOnly testData `shouldBe` check

        it "encode address 1" $
            encodedAddress testAddr1 `shouldBe` encAddr1
        it "encode address 2" $
            encodedAddress testAddr2 `shouldBe` encAddr2
        it "encode address 3" $
            encodedAddress testAddr3 `shouldBe` encAddr3
        it "encode address 4" $
            encodedAddress testAddr4 `shouldBe` encAddr4

        it "calc uplink AP field 1" $
            apFieldForUpFormat testMsg1 testAddr1 `shouldBe` ap1
        it "calc uplink AP field 2" $
            apFieldForUpFormat testMsg1 testAddr2 `shouldBe` ap2
        it "calc uplink AP field 3" $
            apFieldForUpFormat testMsg2 testAddr3 `shouldBe` ap1
        it "calc uplink AP field 4" $
            apFieldForUpFormat testMsg2 testAddr4 `shouldBe` ap2

        it "calc downlink AP field 1" $
            apFieldForDownFormat testMsg3 testAddrDf1 `shouldBe` ap1
        it "calc downlink AP field 2" $
            apFieldForDownFormat testMsg3 testAddrDf2 `shouldBe` ap3
        it "calc downlink AP field 3" $
            apFieldForDownFormat testMsg4 testAddrDf3 `shouldBe` ap1
        it "calc downlink AP field 4" $
            apFieldForDownFormat testMsg4 testAddrDf4 `shouldBe` ap3

        it "check downlink AP field 1" $
            apFieldForDownFormat [ 0x98, 0x60, 0x57, 0xE0, 0x2C, 0xC3, 0x71, 0xC3, 0x2C, 0x20, 0xD6, 0x40, 0x48, 0x8D ] 0 `shouldBe` 0
        it "check downlink AP field 2" $
            apFieldForDownFormat [ 0x00, 0x00, 0x00, 0xE0, 0x2C, 0xC3, 0x71, 0xC3, 0x2C, 0x20, 0xD6, 0x40, 0x48, 0x8D ] 0x576098 `shouldBe` 0
        it "check downlink AP field 3" $
            apFieldForDownFormat [ 166, 85, 122, 35, 32, 77, 93 ] 0 `shouldBe` 0
        it "check downlink AP field 4" $
            apFieldForDownFormat [ 0x8D, 0x07, 0x3F, 0x31, 0x22, 0x11, 0x5F ] 0 `shouldBe` 0
        it "check downlink AP field 5" $
            apFieldForDownFormat  [ 0x00, 0x00, 0x00, 0x31, 0x22, 0x11, 0x5F ] 0x3F078D `shouldBe` 0
        it "check downlink AP field 6" $
            apFieldForDownFormat  [ 0x00, 0x00, 0x00, 0x00, 0x00, 0xa4, 0x30, 0x00, 0xb0, 0x8d, 0x37, 0x0b, 0x00, 0xA0 ] 0x98F94F `shouldBe` 0

        it "check CRC24 (1)" $
            crc24 testMsg1 `shouldBe` crc1
        it "check CRC24 (2)" $
            crc24 testMsg2 `shouldBe` crc2
        it "check CRC24 (3)" $
            crc24 testMsg3 `shouldBe` crc3
        it "check CRC24 (4)" $
            crc24 testMsg4 `shouldBe` crc4

