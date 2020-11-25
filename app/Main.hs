{-

CRC-24 description:
  Name : CRC-24/ADS-B
 Width : 24
  Poly : FFF409
  Init : 000000
 RefIn : false
RefOut : false
XorOut : address(*)
 Check : A05E66

(Check = CRC-24 for the «123456789» string or { 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 })

(*)
- modified address from AA field for mode-S uplink or address for downlink
- modified address 0xFFFFFF for All-Call in mode-S (uplink),
- interrogator identifier from II field for respondes (the 24 bits, where the last 4 bits is the identifier and the 20 bits have zero value),
- 0 for autogenerates squitters

From the "MINIMUM OPERATIONAL PERFORMANCE SPECIFICATION FOR
SECONDARY SURVEILLANCE RADAR MODE S TRANSPONDERS":

>>>>>
The following combinations of texts and interrogation
addresses AA will result in AP as shown:
UF=4, all fields = 0, AA = CO 51 F6 {HEX} : AP = all ZEROs.
UF=4, all fields = 0, AA = 3F AB F2 {HEX} : AP = AA AA AA {HEX}.
UF=20, all fields = 0, AA = AC C5 55 {HEX} : AP = all ZEROs.
UF=20, all fields = 0, AA = 53 3F 51 {HEX} : AP = AA AA AA {HEX}.

DF=5, all fields = 0, AA = 20 78 CE {HEX} : AP = all ZEROs.
DF=5, all fields = 0, AA = 75 2D 9B {HEX} : AP = 55 55 55 {HEX}.
DF=21, all fields = 0, AA = 0B 15 4F {HEX} : AP = all ZEROs.
DF=2l, all fields = 0, AA = 5E 40 1A {HEX} : AP = 55 55 55 {HEX}.
<<<<<

For encoding AP field in the MODE-S uplink:
1.	Calculate CRC-24 for the message with zero values in the least 3 bytes.
2.	Encode the interrogator MODE-S address. Use the 0xFFFFFF address for the all-call (UF11).
3.	Calculatr AP field: <CRC24> XOR <modified address>.

Don`t need the address encoding for the АР field in the MODE-S downlink. Use the address as is. Interrogator identifier may be as addres (if it was in the interrogation)
The address must be encoded for the AP field in the MODE-S uplink. The address  must be multiplied om polynom CRC24, then most significant 24 bits is used.

For AA = C0 51 F6 encoded address is: 80665F
For AA = 3F AB F2 encoded address is: 2ACCF5
For AA = AC C5 55 encoded address is: C88294
For AA = 53 3F 51 encoded address is: 62283E

CRC24 for transmitted data:
UF/DF 4:    0x20 00 00 00 00 00 00,                         CRC-24 = 80665F
UF/DF 5:    0x28 00 00 00 00 00 00,                         CRC-24 = 2078CE
UF/DF 20:   0xA0 00 00 00 00 00 00 00 00 00 00 00 00 00,    CRC-24 = C88294
UF/DF 21:   0xA8 00 00 00 00 00 00 00 00 00 00 00 00 00,    CRC-24 = 0B154F

The encoded all-call address (0xFFFFFF) = 0xAAAC07
UF11 with zero fields = 0x58000000, CRC-24 = 0xE0EF0D, AP = 0x4A430A

Example squitter DF11:
0x 5F 11 22 31 3F 07 8D
0x5F: DF11 и поле CA = 3
0x112231 – MODE-S address
0x3F078D – CRC-24

so crc24 [0x8d, 0x07, 0x3f, 0x31, 0x22, 0x11, 0x5f] = 0

-}

module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Console.GetOpt
import Numeric (readHex, showHex)
import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shift, xor)
import Text.Printf

import Ads_b
import Opts

main :: IO ()
main = do
    (flags, rest) <- getArgs >>= compilerOpts
    print $ "flags: " ++ (show flags)
    print $ "rest: " ++ (show rest)
    print $ "hasVersion: " ++ show (hasVersion flags)
    let addressModeS' = addressModeS flags
    printf "Address: 0x%06X\n" addressModeS'
    let fname' = fname flags
    print $ "File name: " ++ fname'
    content <- if null fname' then return $ head rest else readFile fname'
    print $ intListFromHex content
    if (hasVersion flags) then
        putStrLn "Version: 0.1"
        else return ()
    if (hasCheckCrc flags) then
        printf "CRC24: 0x%06X\n" ((crc24 . byteListFromInt . intListFromHex) content)
        else return ()
    if (hasCalcCrc flags) then
        printf "CRC24: 0x%06X\n" ((crc24DataOnly . byteListFromInt . intListFromHex) content)
        else return ()
    if (hasEncodeAddress flags) then
        printf "Encoded Address: 0x%06X\n" (encodedAddress addressModeS')
        else return ()
    if (hasCalcUplinkApField flags) then
        printf "Uplink AP field: 0x%06X\n" (apFieldForUpFormat ((byteListFromInt . intListFromHex) content) addressModeS')
        else return ()


    {-print $ showHex ((crc24 . byteListFromInt . intListFromHex) content) ""-}

intListFromHex :: String    -- hex bytes string (a least byte in the head)
                  -> [Int]  -- list of bytes
intListFromHex hexStr = map (fst . head . readHex) (words hexStr)

byteListFromInt :: [Int] -> [Word8]
byteListFromInt = map fromIntegral

testData :: [Word8]
testData = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]
testData' :: [Word8]
testData' = [0, 0, 0, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]

testUf1 :: [Word8]
testUf1 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
addr1 :: Word32
addr1 = 0x00C051F6
res1 = 0

testUf2 :: [Word8]
testUf2 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20]
addr2 :: Word32
addr2 = 0x003FABF2
res2 = 0xAAAAAA

testUf3 :: [Word8]
testUf3 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0]
addr3 :: Word32
addr3 = 0xACC555
res3 = 0

testUf4 :: [Word8]
testUf4 = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0]
addr4 :: Word32
addr4 = 0x533F51
res4 = 0xAAAAAA

