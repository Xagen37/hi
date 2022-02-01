{-# LANGUAGE ScopedTypeVariables #-}
module HW3.Pretty
  ( prettyValue
  ) where

import Data.ByteString (unpack)
import Data.Foldable (Foldable (toList))
import qualified Data.Map
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), floatingOrInteger, formatScientific,
                        fromRationalRepetendUnlimited)
import Data.Word (Word8)
import Numeric (showHex)
import Prettyprinter (Doc, Pretty (pretty), concatWith, surround, viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

import HW3.Base (HiAction (..), HiFun (..), HiValue (..))

-- | Prettyprints given HiValue.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueString hiStr) = viaShow hiStr
prettyValue (HiValueTime time)    = pretty "parse-time(\"" <> viaShow time <> pretty "\")"
prettyValue HiValueNull           = pretty "null"
prettyValue (HiValueFunction hiF) = printHiFunc hiF
  where
    printHiFunc HiFunAdd            = pretty "add"
    printHiFunc HiFunDiv            = pretty "div"
    printHiFunc HiFunMul            = pretty "mul"
    printHiFunc HiFunSub            = pretty "sub"
    printHiFunc HiFunNot            = pretty "not"
    printHiFunc HiFunAnd            = pretty "and"
    printHiFunc HiFunOr             = pretty "or"
    printHiFunc HiFunLessThan       = pretty "less-than"
    printHiFunc HiFunGreaterThan    = pretty "greater-than"
    printHiFunc HiFunEquals         = pretty "equals"
    printHiFunc HiFunNotLessThan    = pretty "not-less-than"
    printHiFunc HiFunNotGreaterThan = pretty "not-greater-than"
    printHiFunc HiFunNotEquals      = pretty "not-equals"
    printHiFunc HiFunIf             = pretty "if"
    printHiFunc HiFunLength         = pretty "length"
    printHiFunc HiFunToUpper        = pretty "to-upper"
    printHiFunc HiFunToLower        = pretty "to-lower"
    printHiFunc HiFunReverse        = pretty "reverse"
    printHiFunc HiFunTrim           = pretty "trim"
    printHiFunc HiFunList           = pretty "list"
    printHiFunc HiFunRange          = pretty "range"
    printHiFunc HiFunFold           = pretty "fold"
    printHiFunc HiFunPackBytes      = pretty "pack-bytes"
    printHiFunc HiFunUnpackBytes    = pretty "unpack-bytes"
    printHiFunc HiFunEncodeUtf8     = pretty "encode-utf8"
    printHiFunc HiFunDecodeUtf8     = pretty "decode-utf8"
    printHiFunc HiFunZip            = pretty "zip"
    printHiFunc HiFunUnzip          = pretty "unzip"
    printHiFunc HiFunSerialise      = pretty "serialise"
    printHiFunc HiFunDeserialise    = pretty "deserialise"
    printHiFunc HiFunRead           = pretty "read"
    printHiFunc HiFunWrite          = pretty "write"
    printHiFunc HiFunMkDir          = pretty "mkdir"
    printHiFunc HiFunChDir          = pretty "cd"
    printHiFunc HiFunParseTime      = pretty "parse-time"
    printHiFunc HiFunRand           = pretty "rand"
    printHiFunc HiFunEcho           = pretty "echo"
    printHiFunc HiFunCount          = pretty "count"
    printHiFunc HiFunKeys           = pretty "keys"
    printHiFunc HiFunValues         = pretty "values"
    printHiFunc HiFunInvert         = pretty "invert"

prettyValue (HiValueDict m) = pretty '{' <+>
                              concatWith (surround $ pretty ", ")
                                (map
                                  (\(k, v) -> prettyValue k <> pretty ':' <+> prettyValue v)
                                  (Data.Map.assocs m)) <+>
                              pretty '}'

prettyValue (HiValueAction act) = printHiAct act
  where
    printActArgs :: Doc AnsiStyle -> Doc AnsiStyle
    printActArgs doc = surround doc (pretty "(") (pretty ")")

    printHiAct :: HiAction -> Doc AnsiStyle
    printHiAct (HiActionRead pth)      = pretty "read"  <> printActArgs (viaShow pth)
    printHiAct (HiActionWrite pth str) = pretty "write" <>
                                         printActArgs
                                           (viaShow pth <+> prettyValue (HiValueBytes str))
    printHiAct (HiActionMkDir pth)     = pretty "mkdir" <> printActArgs (viaShow pth)
    printHiAct (HiActionChDir pth)     = pretty "cd"    <> printActArgs (viaShow pth)
    printHiAct HiActionCwd             = pretty "cwd"
    printHiAct HiActionNow             = pretty "now"
    printHiAct (HiActionRand a b)      = pretty "rand"  <>
                                         printActArgs (pretty a <> pretty "," <+> pretty b)
    printHiAct (HiActionEcho str)      = pretty "echo"  <> printActArgs (viaShow str)

prettyValue (HiValueList lst) = pretty '[' <+>
                                concatWith (surround $ pretty ", ")
                                  (map prettyValue $ toList lst) <+>
                                pretty ']'

prettyValue (HiValueBytes bytes) = pretty "[#" <+>
                                   foldr
                                     ((<+>) . pretty . customStringer)
                                     (pretty "")
                                     (unpack bytes) <>
                                   pretty "#]"
  where
    customStringer :: Word8 -> String
    customStringer w = if w < 16 then '0' : showHex w "" else showHex w ""

prettyValue (HiValueBool hiBool) = printHiBool hiBool
  where
    printHiBool :: Bool -> Doc AnsiStyle
    printHiBool True  = pretty "true"
    printHiBool False = pretty "false"

prettyValue (HiValueNumber hiNum) =
  let (sc, period) = fromRationalRepetendUnlimited hiNum
    in case period of
      Nothing -> case floatingOrInteger sc of
        Left (_ :: Double)     -> pretty $ formatScientific Fixed Nothing sc
        Right (int :: Integer) -> pretty int
      Just _ -> printHiRatio $ quotRem (numerator hiNum) (denominator hiNum)
        where
          prettyRational :: Integer -> Integer -> Doc ann
          prettyRational int frac = pretty int <> pretty '/' <> pretty frac

          printHiRatio :: (Integer, Integer) -> Doc AnsiStyle
          printHiRatio (0, r)   = prettyRational r (denominator hiNum)
          printHiRatio (int, r)
            | r > 0     = pretty int <> pretty " + " <> prettyRational r (denominator hiNum)
            | r < 0     = pretty int <> pretty " - " <> prettyRational (abs r) (denominator hiNum)
            | otherwise = pretty int
