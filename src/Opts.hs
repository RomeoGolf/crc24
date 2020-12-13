{- |
Module      : Opts
Description : contains functions for a command line options parsing
Copyright   : (c) RomeoGolf, 2020
License     : MIT License
Maintainer  : triangulumsoft@gmail.com
Stability   : experimental

This module is dedicated to parse a program command line argumens
-}

module Opts
    (
      defaultAddressModeS
    , addressModeS
    , compilerOpts
    , argFname
    , fname
    , helpHeader
    , flagDescr
    , hasVersion
    , hasHelp
    , hasCheckCrc
    , hasCheckCrcUplink
    , hasCheckCrcDownlink
    , hasCalcCrc
    , hasCalcCrcUplink
    , hasCalcCrcDownlink
    , hasEncodeAddress
    , hasCalcUplinkApField
    , hasShowInput
    , intListFromHex
    , byteListFromInt
    , Flag (..)
    )where

import System.Console.GetOpt
import Data.Word (Word8, Word32)
import Control.Applicative
import Numeric (readHex, showHex)

-- | This address is used if an address is not defined by the @'-a'@ option
defaultAddressModeS :: Word32
defaultAddressModeS = 0x00FFFFFF

-- | Permissible values for options
data Flag
    = CheckCrc
    | CheckCrcUplink
    | CheckCrcDownlink
    | CalcCrc
    | CalcCrcUplink
    | CalcCrcDownlink
    | EncodeAddress
    | CalcUplinkApField
    | ShowInput
    | Input String
    | AddrModeS String
    | ArgFile String
    | Version
    | Help
    deriving (Eq, Ord, Show)

-- | Descriptor for command line options
flagDescr =
       [Option []    ["check-crc"]              (NoArg CheckCrc)
            "Check CRC-24 for an input data."
       ,Option []    ["check-crc-uplink"]       (NoArg CheckCrcUplink)
            "Check CRC-24 for an input data."
       ,Option []    ["check-crc-downlink"]     (NoArg CheckCrcDownlink)
            "Check CRC-24 for an input data."

       ,Option []    ["calc-crc"]               (NoArg CalcCrc)
            "Calculate CRC-24 for an input data."
       ,Option []    ["calc-crc-uplink"]        (NoArg CalcCrcUplink)
            "Calculate CRC-24 for an input data."
       ,Option []    ["calc-crc-downlink"]      (NoArg CalcCrcDownlink)
            "Calculate CRC-24 for an input data."

       ,Option ['e'] ["encode-addr"]  (NoArg EncodeAddress)
            "Encode MODE-S uplink address."
       ,Option []    ["calc-ap"]      (NoArg CalcUplinkApField)
            "Calculate MODE-S uplink AP field."

       ,Option ['s'] ["show-input"]   (NoArg ShowInput)
            "Show input data."

       ,Option ['f'] ["file"]         (ReqArg Input "FILE")
            "Input file."
       ,Option ['a'] ["address"]      (ReqArg AddrModeS "ADDRESS")
            "MODE-S aircraft address."
       ,Option [] ["arg-file"]        (ReqArg ArgFile "ARGFILE")
            "The text file contained commandline arguments."

       ,Option ['v'] ["version"]      (NoArg Version)
            "Show version number"
       ,Option ['h', '?'] ["help"]    (NoArg Help)
            "Print this help message"
       ]
-- | Header for a help message (@'-h'@ option)
helpHeader = "Usage: crc24 [OPTION...] [DATA]"

-- | This function gets commandline options
compilerOpts :: [String]            -- ^ command line arguments
        -> IO ([Flag], [String])    -- ^ successfully parsed options
                                    -- and a rest of commanf line
compilerOpts argv =
    case getOpt Permute flagDescr argv of
        (o,n,[]  ) -> return (o,n)
        (_,_,errs) -> ioError (userError (concat errs
            ++ usageInfo helpHeader flagDescr))

hasCheckCrc, hasCheckCrcUplink, hasCheckCrcDownlink,
    hasCalcCrc, hasCalcCrcUplink, hasCalcCrcDownlink,
    hasEncodeAddress, hasShowInput, hasCalcUplinkApField,
    hasHelp, hasVersion :: [Flag] -> Bool
-- | Is @'--check-crc'@ option presents
hasCheckCrc             = elem CheckCrc
-- | Is @'--check-crc-uplink'@ option presents
hasCheckCrcUplink       = elem CheckCrcUplink
-- | Is @'--check-crc-downlink'@ options presents
hasCheckCrcDownlink     = elem CheckCrcDownlink
-- | Is @'--calc-crc'@ option presents
hasCalcCrc              = elem CalcCrc
-- | Is @'--calc-crc-uplink'@ option presents
hasCalcCrcUplink        = elem CalcCrcUplink
-- | Is @'--calc-crc-downlink'@ option presents
hasCalcCrcDownlink      = elem CalcCrcDownlink
-- | Is @'--encode-addr'@ option presents
hasEncodeAddress        = elem EncodeAddress
-- | Is @'--calc-ap'@ option presents
hasCalcUplinkApField    = elem CalcUplinkApField
-- | Is @'-h'@ or @'--help'@ options presents
hasHelp                 = elem Help
-- | Is @'-v'@ option presents
hasVersion              = elem Version
-- | Is @'-s'@ option presents
hasShowInput            = elem ShowInput

argAddress, argFile, argArgFile :: Flag -> Maybe String
argAddress (AddrModeS addr) = Just addr
argAddress _ = Nothing
argFile (Input file) = Just file
argFile _ = Nothing
argArgFile (ArgFile file) = Just file
argArgFile _ = Nothing

-- | This function gets MODE-S address from an @'--address'@ option if it presents
addressModeS :: [Flag] -> Maybe Word32
addressModeS flags = read <$> firstAddress flags
    where
        firstAddress :: [Flag] -> Maybe String
        firstAddress flags = foldr (\x y -> y <|> argAddress x) Nothing flags

-- | This function gets file name to read an input data
-- from a @'--file'@ option if it presents
fname :: [Flag] -> Maybe String
fname = foldr (\x y -> y <|> argFile x) Nothing

-- | This function gets file name to read an command line options
-- from an @'--arg-file'@ option if it presents
argFname :: [Flag] -> Maybe String
argFname = foldr (\x y -> y <|> argArgFile x) Nothing

-- | This function converts a hex bytes string (an input data) to an Int list
intListFromHex :: String    -- hex bytes string (a least byte in the head)
                  -> [Int]  -- list of bytes
intListFromHex hexStr = map (fst . head . readHex) (words hexStr)

-- | This function is for a type casting: Int to Word8
byteListFromInt :: [Int] -> [Word8]
byteListFromInt = map fromIntegral


