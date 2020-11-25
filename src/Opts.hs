
module Opts where

import System.Console.GetOpt
import Data.Word (Word8, Word32)
import Control.Applicative

defaultAddressModeS :: Word32
defaultAddressModeS = 0x00FFFFFF

defaultFname :: String
defaultFname = ""

data Flag
    = CheckCrc
    | CalcCrc
    | EncodeAddress
    | CalcUplinkApField
    | Input String
    | AddrModeS String
    | Version
    | Help
    deriving (Eq, Ord, Show)

flagDescr =
       [Option []    ["check-crc"]    (NoArg CheckCrc)
            "Check CRC-24 for an input data."
       ,Option []    ["calc-crc"]     (NoArg CalcCrc)
            "Calculate CRC-24 for an input data."
       ,Option ['e'] ["encode-addr"]  (NoArg EncodeAddress)
            "Encode MODE-S uplink address."
       ,Option []    ["calc-ap"]      (NoArg CalcUplinkApField)
            "Calculate MODE-S uplink AP field."

       ,Option ['f'] ["file"]         (ReqArg Input "FILE")
            "Input file."
       ,Option ['a'] ["address"]      (ReqArg AddrModeS "ADDRESS")
            "MODE-S aircraft address."

       ,Option ['v'] ["version"]      (NoArg Version)
            "Show version number"
       ,Option ['h', '?'] ["help"]    (NoArg Help)
            "Print this help message"
       ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute flagDescr argv of
       (o,n,[]  ) -> return (o,n)
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header flagDescr))
    where header = "Usage: <this-exe> [OPTION...] files..."

hasCheckCrc, hasCalcCrc, hasEncodeAddress,
    hasCalcUplinkApField, hasHelp, hasVersion :: [Flag] -> Bool
hasCheckCrc             = elem CheckCrc
hasCalcCrc              = elem CalcCrc
hasEncodeAddress        = elem EncodeAddress
hasCalcUplinkApField    = elem CalcUplinkApField
hasHelp                 = elem Help
hasVersion              = elem Version

argAddress, argFile :: Flag -> Maybe String
argAddress (AddrModeS addr) = Just addr
argAddress _ = Nothing
argFile (Input file) = Just file
argFile _ = Nothing

addressModeS :: [Flag] -> Word32
addressModeS = maybe defaultAddressModeS read . firstAddress
    where
        firstAddress :: [Flag] -> Maybe String
        firstAddress flags = foldr (\x y -> y <|> argAddress x) Nothing flags

fname :: [Flag] -> String
fname = maybe defaultFname id . firstFname
    where
        firstFname :: [Flag] -> Maybe String
        firstFname flags = foldr (\x y -> y <|> argFile x) Nothing flags



