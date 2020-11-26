
module Ads_b
    (
    crc24,
    crc24DataOnly,
    encodedAddress,
    apFieldForUpFormat
    ) where

import Numeric (readHex, showHex)
import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shift, xor)


preparedData :: [Word8]                 -- input list of bytes
                -> (Word32, [Word8])    -- initial buffer and the rest list
preparedData (x0:x1:x2:xs) = let
    initBuf = fromIntegral x0 `shift` 24
                .|. fromIntegral x1 `shift` 16
                .|. fromIntegral x2 `shift` 08
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
    buf' = buf .|. fromIntegral x
    processedBuf :: Word32 -> Int -> Word32
    processedBuf b 0 = b
    processedBuf b cnt = let
        cBit = (b .&. maskC) /= 0
        b' = b `shift` 1
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
                   -> Word32    -- the CRC24 polynom
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

apFieldForUpFormat :: [Word8]     -- the input bytes
                      -> Word32   -- the MODE-S address
                      -> Word32   -- AP field
apFieldForUpFormat bytes addr = let
    crc = crc24 bytes
    addr' = encodedAddress addr
    in crc `xor` addr'


{-
byte[] msg = { 0x98, 0x60, 0x57, 0xE0, 0x2C, 0xC3, 0x71, 0xC3, 0x2C, 0x20, 0xD6, 0x40, 0x48, 0x8D };
simpleTestDF(msg, 0);

byte[] msg2 = { 0x00, 0x00, 0x00, 0xE0, 0x2C, 0xC3, 0x71, 0xC3, 0x2C, 0x20, 0xD6, 0x40, 0x48, 0x8D };
simpleTestDF(msg2, 0x576098);

byte[] msg5 = { 166, 85, 122, 35, 32, 77, 93 };
simpleTestDF(msg5, 0);

byte[] msg_real = { 0x8D, 0x07, 0x3F, 0x31, 0x22, 0x11, 0x5F };
simpleTestDF(msg_real, 0);

byte[] msg_real_zt = { 0x00, 0x00, 0x00, 0x31, 0x22, 0x11, 0x5F };
simpleTestDF(msg_real_zt, 0x3F078D);

byte[] msg_test1_df = { 0xce, 0x78, 0x20, 0x00, 0x00, 0x00, 0x28 };
simpleTestDF(msg_test1_df, 0);

byte[] msg_test2_df = { 0x9b, 0x2d, 0x75, 0x00, 0x00, 0x00, 0x28 };
simpleTestDF(msg_test2_df, 0x555555);

byte[] msg_test3_df = { 0x4f, 0x15, 0x0b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA8 };
simpleTestDF(msg_test3_df, 0);

byte[] msg_test4_df = { 0x1a, 0x40, 0x5e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA8 };
simpleTestDF(msg_test4_df, 0x555555);

// A0 00 0B 37 8D B0 00 30 A4 00 00
byte[] msg_test5_xz = { 0x00, 0x00, 0x00, 0x00, 0x00, 0xa4, 0x30, 0x00, 0xb0, 0x8d, 0x37, 0x0b, 0x00, 0xA0 };
simpleTestDF(msg_test5_xz, 0x98F94F);

-------
//byte[] msg_test1_uf = { 0xf6, 0x51, 0xc0, 0x00, 0x00, 0x00, 0x20 };
byte[] msg_test1_uf = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20 };
testUpFormat(msg_test1_uf, 0x00C051F6, 0);

byte[] msg_test2_uf = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x20 };
testUpFormat(msg_test2_uf, 0x003FABF2, 0xAAAAAA);

byte[] msg_test3_uf = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0 };
testUpFormat(msg_test3_uf, 0xACC555, 0);

byte[] msg_test4_uf = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xA0 };
testUpFormat(msg_test4_uf, 0x533F51, 0xAAAAAA);

------
byte[] msg_test1_uf_fwd = { 0x0, 0x0, 0x0, 0x00, 0x00, 0x00, 0x20 };
logTextBlock.Inlines.Add(string.Format("data: {0}\n", byteArrayToStr(msg_test1_uf_fwd)));
UInt32 crc = getCrc24FullLen(msg_test1_uf_fwd);
UInt32 b = getEncodedAddress(0x112231);
logTextBlock.Inlines.Add(string.Format("crc: {0:X6}, addr: {1:X6}, encoded addr: {2:X6}, AP: {3:X6} \n", crc, 0x112231, b, (crc ^ b)));

byte[] msg_test1_uf_2 = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x58 };
logTextBlock.Inlines.Add(string.Format("data: {0}\n", byteArrayToStr(msg_test1_uf_2)));
crc = getCrc24FullLen(msg_test1_uf_2);
b = getEncodedAddress(0xFFFFFF);
logTextBlock.Inlines.Add(string.Format("crc: {0:X6}, addr: {1:X6}, encoded addr: {2:X6}, AP: {3:X6} \n", crc, 0xFFFFFF, b, (crc ^ b)));


//200011903CCE6F
byte[] msg_real_2 = { 0x00, 0x00, 0x00, 0x90, 0x11, 0x00, 0x20 };
logTextBlock.Inlines.Add(string.Format("data: {0}\n", byteArrayToStr(msg_real_2)));
crc = getCrc24FullLen(msg_real_2);
b = getEncodedAddress(0x003FABF2);
logTextBlock.Inlines.Add(string.Format("crc: {0:X6}, addr: {1:X6}, encoded addr: {2:X6}, AP: {3:X6} \n", crc, 0x003FABF2, b, (crc ^ b)));
-}
