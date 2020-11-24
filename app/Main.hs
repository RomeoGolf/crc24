{-

CRC-24 description:
Name:		CRC-24/ADS-B
Width:		24
Poly:		FFF409
Init:		0000
RefIn:		false
RefOut:		false
XorOut:		address(*)
Check:		A05E66 (for the «123456789» string or { 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39 })

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

-}

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

intListFromHex :: String    -- hex bytes string (a least byte in the head)
                  -> [Int]  -- list of bytes
intListFromHex hexStr = map (fst . head . readHex) (words hexStr)

preparedData :: [Word8]                 -- input list of bytes
                -> (Word32, [Word8])    -- initial buffer and the rest list
preparedData (x0:x1:x2:xs) = let
    initBuf = (fromIntegral x0) `shift` 24
                .|. (fromIntegral x1) `shift` 16
                .|. (fromIntegral x2) `shift` 08
    in (initBuf, xs)
preparedData _ = error "The data is too short!"

crc24' :: (Word32, [Word8])     -- initial buffer and the rest list
            -> Word32           -- crc24 in 3 least bytes
crc24' (buf, []) = buf `shift` (-8)
crc24' (buf, x:xs) = let
    maskC :: Word32
    maskC = 0x80000000
    poly :: Word32
    poly = 0xFFF40900
    buf' = buf .|. (fromIntegral x)
    processedBuf :: Word32 -> Int -> Word32
    processedBuf b 0 = b
    processedBuf b cnt = let
        cBit = (b .&. maskC) /= 0
        b' = b `shift` (1)
        b'' = if cBit then b' `xor` poly else b'
        in processedBuf b'' (pred cnt)
    in crc24' (processedBuf buf' 8, xs)

crc24 :: [Word8]        -- input bytes list with 3 zero least bytes
         -> Word32      -- crc24 in 3 leasb bytes
crc24 = crc24' . preparedData . reverse

testData :: [Word8]
testData = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]
testData' :: [Word8]
testData' = [0, 0, 0, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]

crc24DataOnly :: [Word8] -> Word32
crc24DataOnly xs = crc24 $ 0:0:0:xs



encodedAddress' :: Word32       -- the MODE-S address
                   -> Word32    -- the polynom CRC24
                   -> Word32    -- the buffer for a result
                   -> Int       -- the counter for recurrent invoking
                   -> Word32    -- the encoded address
encodedAddress' addr poly buff 0 = buff .&. 0x00FFFFFF     -- least 24 bits
encodedAddress' addr poly buff cnt = let
    maskC :: Word32
    maskC = 0x01000000
    addr' = addr `shift` 1
    poly' = poly `shift` (-1)
    buff' = if addr' .&. maskC /= 0 then buff `xor` poly' else buff
    in encodedAddress' addr' poly' buff' (pred cnt)

encodedAddress :: Word32        -- the MODE-S address
                  -> Word32     -- the encoded address
encodedAddress addr = encodedAddress' addr poly 0 24 where
    poly :: Word32
    poly = 0x01FFF409

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

testUpFormat :: [Word8]     -- the input bytes
                -> Word32   -- the MODE-S address
                -> Word32   -- AP field
testUpFormat bytes addr = let
    crc = crc24 bytes
    addr' = encodedAddress addr
    in crc `xor` addr'
