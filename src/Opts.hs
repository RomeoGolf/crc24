
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
    | ShowInput
    | Input String
    | AddrModeS String
    | ArgFile String
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

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute flagDescr argv of
       (o,n,[]  ) -> return (o,n)
       (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header flagDescr))
    where header = "Usage: <this-exe> [OPTION...] files..."

hasCheckCrc, hasCalcCrc, hasEncodeAddress, hasShowInput,
    hasCalcUplinkApField, hasHelp, hasVersion :: [Flag] -> Bool
hasCheckCrc             = elem CheckCrc
hasCalcCrc              = elem CalcCrc
hasEncodeAddress        = elem EncodeAddress
hasCalcUplinkApField    = elem CalcUplinkApField
hasHelp                 = elem Help
hasVersion              = elem Version
hasShowInput            = elem ShowInput

argAddress, argFile, argArgFile :: Flag -> Maybe String
argAddress (AddrModeS addr) = Just addr
argAddress _ = Nothing
argFile (Input file) = Just file
argFile _ = Nothing
argArgFile (ArgFile file) = Just file
argArgFile _ = Nothing

addressModeS :: [Flag] -> Maybe Word32
addressModeS flags = read <$> firstAddress flags
    where
        firstAddress :: [Flag] -> Maybe String
        firstAddress flags = foldr (\x y -> y <|> argAddress x) Nothing flags

fname :: [Flag] -> Maybe String
fname = foldr (\x y -> y <|> argFile x) Nothing

argFname :: [Flag] -> Maybe String
argFname = foldr (\x y -> y <|> argArgFile x) Nothing



