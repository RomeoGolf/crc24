
module Main where

import System.Environment (getArgs)
import System.IO (readFile)
import System.Exit
import System.Console.GetOpt
import Text.Printf
import Data.Maybe (fromMaybe)
import Control.Monad (when)

import Ads_b
import Opts

main :: IO ()
main = do
    -- get commandline options
    (flags, rest) <- getArgs >>= compilerOpts
    -- get options from a file if it presents
    (flags, rest) <- case argFname flags of
        Nothing    -> return (flags, rest)
        Just fname -> readFile fname >>= compilerOpts . words

    let addressModeS' = fromMaybe defaultAddressModeS (addressModeS flags)

    -- read data (message)
    content <- case fname flags of
        Nothing     -> return $ unwords rest
        Just fname' -> readFile fname'

    when (null flags) $ do
        putStrLn "No options!"
        putStr $ usageInfo helpHeader flagDescr
        exitSuccess

    when (hasVersion flags) $ do
        putStrLn "Version: 0.1"

    when (hasHelp flags) $ do
        putStr $ usageInfo helpHeader flagDescr
        exitSuccess

    when (hasShowInput flags) $ do
            case addressModeS flags of
                Nothing     -> printf "No address in the options!\n\
                    \Default all-call address 0x%06X will be used.\n"
                    defaultAddressModeS
                Just fname' -> printf "Address: 0x%06X\n" addressModeS'
            case fname flags of
                Nothing     -> putStrLn "No data file name"
                Just fname' -> putStrLn $ "Data file name: " ++ fname'
            putStr $ "Input message (hex): " ++ content

    when (hasCheckCrc flags) $
        case (crc24 . byteListFromInt . intListFromHex) content of
            CrcIsOk  -> putStrLn "CRC-24 is OK"
            Fail crc -> printf "CRC-24 is wrong: 0x%06X\n" crc

    when (hasCheckCrcUplink flags) $ do
        case addressModeS flags of
            Nothing     -> printf "Warning: Default all-call address 0x%06X \
            \is used.\n" defaultAddressModeS
            Just fname' -> return ()
        case (crc24XorOut (encodedAddress addressModeS')
                . byteListFromInt . intListFromHex) content of
            CrcIsOk  -> putStrLn "Uplink CRC-24 is OK"
            Fail crc -> printf "Uplink CRC-24 is wrong: 0x%06X\n" crc

    when (hasCheckCrcDownlink flags) $ do
        case addressModeS flags of
            Nothing     -> printf "Warning: Default all-call address 0x%06X \
            \is used.\n" defaultAddressModeS
            Just fname' -> return ()
        case (crc24XorOut addressModeS'
                . byteListFromInt . intListFromHex) content of
            CrcIsOk  -> putStrLn "Downlink CRC-24 is OK"
            Fail crc -> printf "Downlink CRC-24 is wrong: 0x%06X\n" crc

    when (hasCalcCrc flags) $
        printf "CRC24: 0x%06X\n" ((crc24DataOnly . byteListFromInt
            . intListFromHex) content)

    when (hasCalcCrcDownlink flags) $ do
        case addressModeS flags of
            Nothing     -> printf "Warning: Default all-call address 0x%06X \
            \is used.\n" defaultAddressModeS
            Just fname' -> return ()
        printf "CRC24 downlink: 0x%06X\n" $ (crc24DataOnlyXorOut addressModeS'
                . byteListFromInt . intListFromHex) content

    when (hasCalcCrcUplink flags) $ do
        case addressModeS flags of
            Nothing     -> printf "Warning: Default all-call address 0x%06X \
            \is used.\n" defaultAddressModeS
            Just fname' -> return ()
        printf "CRC24 uplink: 0x%06X\n" $
            (crc24DataOnlyXorOut (encodedAddress addressModeS')
                . byteListFromInt . intListFromHex) content

    when (hasEncodeAddress flags) $ do
        case addressModeS flags of
            Nothing     -> printf "Warning: Default all-call address 0x%06X \
            \is used.\n" defaultAddressModeS
            Just fname' -> return ()
        printf "Encoded Address: 0x%06X\n" (encodedAddress addressModeS')

    when (hasCalcUplinkApField flags) $ do
        case addressModeS flags of
            Nothing     -> printf "Warning: Default all-call address 0x%06X \
            \is used.\n" defaultAddressModeS
            Just fname' -> return ()
        printf "Uplink AP field: 0x%06X\n"
            (apFieldForUpFormat ((byteListFromInt
                    . intListFromHex) content) addressModeS')

