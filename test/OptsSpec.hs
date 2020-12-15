module OptsSpec where

import Test.QuickCheck hiding ((.|.), (.&.), shift, xor)
import Test.Hspec

import Opts
import System.Console.GetOpt

{-import System.IO.Error (isUserError)-}
import Data.Word (Word8)

flagsAll = [CheckCrc, CheckCrcUplink, CheckCrcDownlink, CalcCrc, CalcCrcUplink,
        CalcCrcDownlink, EncodeAddress, CalcUplinkApField, ShowInput,
        Input "data.hex", AddrModeS "0x012345", ArgFile "args.txt", Version,
        Help]
flagsEmpty = []

optsAllShort = ["--check-crc", "--check-crc-uplink", "--check-crc-downlink",
    "--calc-crc", "--calc-crc-uplink", "--calc-crc-downlink",
    "-e", "--calc-ap", "-s",
    "-fdata.hex",
    "-a0x012345",
    "--arg-file=args.txt",
    "-v", "-h",
    "31", "32", "33", "34", "35"]

optsAllLong = ["--check-crc", "--check-crc-uplink", "--check-crc-downlink",
    "--calc-crc", "--calc-crc-uplink", "--calc-crc-downlink",
    "--encode-addr", "--calc-ap", "--show-input",
    "--file=data.hex",
    "--address=0x012345",
    "--arg-file=args.txt",
    "--version", "--help",
    "31", "32", "33", "34", "35"]


testDataInput = "31 32 33 34 35 36 37 38 39"

testDataInt :: [Int]
testDataInt = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]

testDataWord8 :: [Word8]
testDataWord8 = [0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39]

optsEmpty = [""]

restStr = ["31", "32", "33", "34", "35"]

optsFail = ["--fail"]

spec :: Spec
spec = do
    describe "Opts" $ do

        it "hasVersion test 1" $
            hasVersion flagsAll `shouldBe` True
        it "hasVersion test 2" $
            hasVersion flagsEmpty `shouldBe` False
        it "hasHelp test 1" $
            hasHelp flagsAll `shouldBe` True
        it "hasHelp test 2" $
            hasHelp flagsEmpty `shouldBe` False
        it "hasCheckCrc test 1" $
            hasCheckCrc flagsAll `shouldBe` True
        it "hasCheckCrc test 2" $
            hasCheckCrc flagsEmpty `shouldBe` False
        it "hasCheckCrcUplink test 1" $
            hasCheckCrcUplink flagsAll `shouldBe` True
        it "hasCheckCrcUplink test 2" $
            hasCheckCrcUplink flagsEmpty `shouldBe` False
        it "hasCheckCrcDownlink test 1" $
            hasCheckCrcDownlink flagsAll `shouldBe` True
        it "hasCheckCrcDownlink test 2" $
            hasCheckCrcDownlink flagsEmpty `shouldBe` False
        it "hasCalcCrc test 1" $
            hasCalcCrc flagsAll `shouldBe` True
        it "hasCalcCrc test 2" $
            hasCalcCrc flagsEmpty `shouldBe` False
        it "hasCalcCrcUplink test 1" $
            hasCalcCrcUplink flagsAll `shouldBe` True
        it "hasCalcCrcUplink test 2" $
            hasCalcCrcUplink flagsEmpty `shouldBe` False
        it "hasCalcCrcDownlink test 1" $
            hasCalcCrcDownlink flagsAll `shouldBe` True
        it "hasCalcCrcDownlink test 2" $
            hasCalcCrcDownlink flagsEmpty `shouldBe` False
        it "hasEncodeAddress test 1" $
            hasEncodeAddress flagsAll `shouldBe` True
        it "hasEncodeAddress test 2" $
            hasEncodeAddress flagsEmpty `shouldBe` False
        it "hasCalcUplinkApField test 1" $
            hasCalcUplinkApField flagsAll `shouldBe` True
        it "hasCalcUplinkApField test 2" $
            hasCalcUplinkApField flagsEmpty `shouldBe` False
        it "hasShowInput test 1" $
            hasShowInput flagsAll `shouldBe` True
        it "hasShowInput test 2" $
            hasShowInput flagsEmpty `shouldBe` False

        it "const test 1" $
            defaultAddressModeS `shouldBe` 0x00FFFFFF
        it "const test 1" $
            helpHeader `shouldBe` "Usage: crc24 [OPTION...] [DATA]"

        it "addressModeS test 1" $
            addressModeS flagsAll `shouldBe` Just 0x012345
        it "addressModeS test 2" $
            addressModeS flagsEmpty `shouldBe` Nothing

        it "fname test 1" $
            fname flagsAll `shouldBe` Just "data.hex"
        it "fname test 2" $
            fname flagsEmpty `shouldBe` Nothing

        it "argFname test 1" $
            argFname flagsAll `shouldBe` Just "args.txt"
        it "argFname test 2" $
            argFname flagsEmpty `shouldBe` Nothing

        it "intListFromHex test" $
            intListFromHex testDataInput `shouldBe` testDataInt
        it "byteListFromInt test" $
            byteListFromInt testDataInt `shouldBe` testDataWord8

        it "compilerOpts empty test" $
            {-compilerOpts opts1`shouldReturn` ([],[[]])-}
            {-compilerOpts opts1 >>= (`shouldBe` ([],[[]]) )-}
            do
                (x, y) <- compilerOpts optsEmpty
                null x `shouldBe` True
                y == [[]] `shouldBe` True

        it "compilerOpts success long options test" $
            do
                (x, y) <- compilerOpts optsAllLong
                x == flagsAll `shouldBe` True
                y `shouldBe` restStr

        it "compilerOpts success short options test" $
            do
                (x, y) <- compilerOpts optsAllShort
                x == flagsAll `shouldBe` True
                y `shouldBe` restStr

        it "compilerOpts fail test" $
            compilerOpts optsFail `shouldThrow` -- isUserError -- anyIOException
                (== userError ("unrecognized option `--fail'\n" ++ usageInfo helpHeader flagDescr))



