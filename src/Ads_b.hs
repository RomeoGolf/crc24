
module Ads_b
    (
      crc24
    , crc24DataOnly
    , crc24XorOut
    , crc24DataOnlyXorOut
    , encodedAddress
    , apFieldForUpFormat
    , apFieldForDownFormat
    , Crc24CheckResult (CrcIsOk, Fail)
    , errorMessagePrepareData
    ) where

import Numeric (readHex, showHex)
import Data.Word (Word8, Word32)
import Data.Bits ((.|.), (.&.), shift, xor)

data Crc24CheckResult = CrcIsOk | Fail Word32   deriving (Show, Eq)

mask24bits = 0x00FFFFFF
errorMessagePrepareData = "The data is too short!"

preparedData :: [Word8]                 -- input list of bytes
                -> (Word32, [Word8])    -- initial buffer and the rest list
preparedData (x0:x1:x2:xs) = let
    initBuf = fromIntegral x0 `shift` 24
                .|. fromIntegral x1 `shift` 16
                .|. fromIntegral x2 `shift` 08
    in (initBuf, xs)
preparedData _ = error errorMessagePrepareData

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
         -> Crc24CheckResult
crc24 msg = let result = (crc24' . preparedData . reverse) msg in
    case result of
        0 -> CrcIsOk
        _ -> Fail result

crc24XorOut ::
            Word32      -- Data for XOR
         -> [Word8]     -- input bytes list with 3 zero least bytes
         -> Crc24CheckResult
crc24XorOut xorData msg = let
    result = (crc24' . preparedData . reverse) msg `xor` (xorData .&. mask24bits) in
    case result of
        0 -> CrcIsOk
        _ -> Fail result

crc24DataOnly :: [Word8] -> Word32
crc24DataOnly xs = (crc24' . preparedData . reverse) $ 0:0:0:xs

crc24DataOnlyXorOut :: Word32 -> [Word8] -> Word32
crc24DataOnlyXorOut xorData xs = (.&.) mask24bits $ crc24DataOnly xs `xor` xorData

encodedAddress' :: Word32       -- the MODE-S address
                   -> Word32    -- the CRC24 polynom
                   -> Word32    -- the buffer for a result
                   -> Int       -- the counter for recurrent invoking
                   -> Word32    -- the encoded address
encodedAddress' addr poly buff 0 = buff .&. mask24bits -- least 24 bits
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

apFieldForUpFormat :: [Word8]     -- the input bytes
                      -> Word32   -- the MODE-S address
                      -> Word32   -- AP field
apFieldForUpFormat bytes addr = let
    bytes' = (tail . tail . tail) bytes
    crc = crc24DataOnly bytes'
    addr' = encodedAddress addr
    in crc `xor` addr'

apFieldForDownFormat :: [Word8]   -- the input bytes
                      -> Word32   -- the MODE-S address
                      -> Word32   -- AP field
apFieldForDownFormat bytes addr = let
    bytes' = (tail . tail . tail) bytes
    crc = crc24DataOnly bytes'
    in crc `xor` addr
