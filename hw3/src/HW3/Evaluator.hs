{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HW3.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith,
                               decompress, defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Lazy as DBSL
import Data.Foldable (Foldable (toList))
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy, uncons)
import qualified Data.Map as DM
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (Empty, (:<|)), (><))
import qualified Data.Sequence as DS
import Data.Text (Text)
import qualified Data.Text as DT
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Tuple (swap)
import Data.Word (Word8)
import Text.Read (readMaybe)

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction),
                 HiValue (..))

-- | Accepts HiExpr and evaluates it.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue val) = return (Right val)
eval (HiExprDict lst) = do
  let (keys, values) = unzip lst
  evaledKeys <- mapM eval keys
  evaledVals <- mapM eval values
  case sequence evaledKeys of
    Left err -> return (Left err)
    Right ks -> case sequence evaledVals of
      Left err -> return (Left err)
      Right vs -> return (Right $ HiValueDict $ DM.fromList $ zip ks vs)
eval (HiExprApply func args) = do
  evaledFunc <- eval func
  case evaledFunc of
    Left err                  -> return (Left err)
    Right (HiValueFunction f) ->
      if f `elem` [HiFunAnd, HiFunOr, HiFunIf]
        then mapLazy f args
        else evalHelper applyFunc f args
    Right (HiValueString str) -> evalHelper indexStr str args
    Right (HiValueList lst)   -> evalHelper indexList lst args
    Right (HiValueBytes str)  -> evalHelper indexBytes str args
    Right (HiValueDict m)     -> evalHelper getFromMap m args
    Right _                   -> return (Left HiErrorInvalidFunction)
eval (HiExprRun runnable) = do
  evaledRun <- eval runnable
  case evaledRun of
    Left err                  -> return (Left err)
    Right (HiValueAction act) -> Right <$> runAction act
    Right _                   -> return $ Left HiErrorInvalidArgument

-- | Maps lazy HiFun to corresponding implementation.
mapLazy
  :: HiMonad m
  => HiFun
  -> [HiExpr]
  -> m (Either HiError HiValue)
mapLazy HiFunAnd = applyLazyBiBool (\val -> val == HiValueBool False || val == HiValueNull)
mapLazy HiFunOr  = applyLazyBiBool (\val -> val /= HiValueBool False && val /= HiValueNull)
mapLazy HiFunIf  = applyLazyIf
mapLazy _        = const $ return $ Left HiErrorInvalidArgument  -- impossible

-- | Evaluates arguments for strict function and run it.
evalHelper
  :: HiMonad m
  => (a -> [HiValue] -> Either HiError HiValue)  -- ^ Object mapper function
  -> a                                           -- ^ Object to map (function, list, etc.)
  -> [HiExpr]                                    -- ^ List of arguments
  -> m (Either HiError HiValue)                  -- ^ Execution result
evalHelper objFun obj args = do
  evaledArgs <- mapM eval args
  case sequence evaledArgs of
        Left err   -> return (Left err)
        Right vals ->
          let res = objFun obj vals in
          return res

-------------------------- LAZY ---------------------------

-- | Executes lazy "if", i.e.
-- if (true, A, B) does not evaluate B;
-- if (false, A, B) does not evaluate A.
applyLazyIf :: HiMonad m => [HiExpr] -> m (Either HiError HiValue)
applyLazyIf [cond, a, b] = do
  evaledCond <- eval cond
  case evaledCond of
    Left err -> return $ Left err
    Right (HiValueBool res) -> do
      evaledArg <- if res then eval a else eval b
      case evaledArg of
        Left err -> return $ Left err
        Right hv -> return $ Right hv
    Right _ -> return $ Left HiErrorInvalidArgument
applyLazyIf _ = return $ Left HiErrorArityMismatch

-- | Executes lazy binary bool function, such as "and" or "or".
applyLazyBiBool
  :: HiMonad m
  => (HiValue -> Bool)           -- ^ Function, checking if the next args should be evaluated
  -> [HiExpr]                    -- ^ List of arguments
  -> m (Either HiError HiValue)  -- ^ Function result
applyLazyBiBool check [a, b] = do
  evaledFirst <- eval a
  case evaledFirst of
    Left err -> return $ Left err
    Right valA -> if check valA then return $ Right valA else do
      evaledSecond <- eval b
      case evaledSecond of
        Left err   -> return $ Left err
        Right valB -> return $ Right valB
applyLazyBiBool _ _ = return $ Left HiErrorArityMismatch

------------------ Indexing and slicing -------------------

-- | Gets value from map by key.
-- Returns value if such key is present in map or null otherwise.
getFromMap :: DM.Map HiValue HiValue -> [HiValue] -> Either HiError HiValue
getFromMap m [key] = case DM.lookup key m of
  Nothing  -> Right HiValueNull
  Just val -> Right val
getFromMap _ _ = Left HiErrorArityMismatch

-- | If given one number,
-- returns element at given position or null is index was invalid.
-- if given two numbers or nulls, gets slice of the list.
indexList :: Seq HiValue -> [HiValue] -> Either HiError HiValue
indexList lst = indexAll lst
                DS.length
                (DS.lookup 0)
                DS.take
                DS.drop
                HiValueList
                id

-- | If given one number,
-- returns character at given position or empty string is index was invalid.
-- If given two numbers or nulls, gets substring.
indexStr :: Text -> [HiValue] -> Either HiError HiValue
indexStr str = indexAll (DT.unpack str)
               length
               (\txt -> case uncons txt of
                 Nothing    -> Nothing
                 Just(c, _) -> Just c)
               take
               drop
               (HiValueString . DT.pack)
               (HiValueString . DT.singleton)

-- | If given one argument,
-- returns byte at given position or null is index was invalid.
-- If given two numbers or nulls, gets substring.
indexBytes :: DBS.ByteString -> [HiValue] -> Either HiError HiValue
indexBytes str = indexAll (DBS.unpack str)
                 length
                 (\txt -> case uncons txt of
                   Nothing    -> Nothing
                   Just(c, _) -> Just c)
                 take
                 drop
                 (HiValueBytes . DBS.pack)
                 (HiValueNumber . toRational)

-------------------- GENERAL FUNCTIONS --------------------

-- | General function helper.
-- Maps HiFun to the corresponding implementation or helper.
applyFunc :: HiFun -> [HiValue] -> Either HiError HiValue
applyFunc func = case func of
---------- Arithmetic ----------
  HiFunAdd            -> applyDualFunc func
  HiFunDiv            -> applyDualFunc func
  HiFunMul            -> applyDualFunc func
  HiFunSub            -> applyDualFunc func
------ Bools and compares ------
  HiFunNot            -> applyUnaryBoolFunc (Right . HiValueBool . not)
  HiFunAnd            -> applyBiBoolFunc (\val -> val == HiValueBool False || val == HiValueNull)
  HiFunOr             -> applyBiBoolFunc (\val -> val /= HiValueBool False && val /= HiValueNull)
  HiFunLessThan       -> applyBiCompFunc (\a b -> Right $ HiValueBool $ a < b)
  HiFunGreaterThan    -> applyBiCompFunc (\a b -> Right $ HiValueBool $ a > b)
  HiFunEquals         -> applyBiCompFunc (\a b -> Right $ HiValueBool $ a == b)
  HiFunNotLessThan    -> applyBiCompFunc (\a b -> Right $ HiValueBool $ a >= b)
  HiFunNotGreaterThan -> applyBiCompFunc (\a b -> Right $ HiValueBool $ a <= b)
  HiFunNotEquals      -> applyBiCompFunc (\a b -> Right $ HiValueBool $ a /= b)
  HiFunIf             -> applyIfFunc (\cond true false -> Right $ if cond then true else false)
------- String functions -------
  HiFunLength         -> applyDualFunc func
  HiFunToUpper        -> applyStrFunc (Right . HiValueString . DT.toUpper)
  HiFunToLower        -> applyStrFunc (Right . HiValueString . DT.toLower)
  HiFunReverse        -> applyDualFunc func
  HiFunTrim           -> applyStrFunc (Right . HiValueString . DT.strip)
-------- List functions --------
  HiFunList           -> applyList
  HiFunRange          -> applyBiRatioFunc (\a b -> Right $ HiValueList (DS.fromList $ map HiValueNumber [a..b]))
  HiFunFold           -> applyFold
----- Bytestring functions -----
  HiFunPackBytes      -> applyPack
  HiFunUnpackBytes    -> applyByteFunc (Right . HiValueList . DS.fromList . map (HiValueNumber . toRational . toInteger) . DBS.unpack)
  HiFunEncodeUtf8     -> applyEncode
  HiFunDecodeUtf8     -> applyByteFunc (\bytes -> let res = decodeUtf8' bytes in case res of
                                                    Left _    -> Right HiValueNull
                                                    Right txt -> Right $ HiValueString txt )
  HiFunZip            -> applyByteFunc (Right . HiValueBytes . DBSL.toStrict . compressWith defaultCompressParams { compressLevel = bestCompression } . DBSL.fromStrict)
  HiFunUnzip          -> applyByteFunc (Right . HiValueBytes . DBSL.toStrict . decompress . DBSL.fromStrict)
  HiFunSerialise      -> applySerialise
  HiFunDeserialise    -> applyByteFunc (\bytes -> let res = deserialiseOrFail (DBSL.fromStrict bytes) in case res of
                                                    Left _    -> Right HiValueNull
                                                    Right val -> Right val )
------------ Actions -----------
  HiFunRead           -> applyUnAct (HiActionRead . DT.unpack)
  HiFunWrite          -> applyWriteAct HiActionWrite
  HiFunMkDir          -> applyUnAct (HiActionMkDir . DT.unpack)
  HiFunChDir          -> applyUnAct (HiActionChDir . DT.unpack)
  HiFunParseTime      -> applyStrFunc (\txt -> let parsed = readMaybe (DT.unpack txt) in case parsed of
                                                  Nothing   -> Right  HiValueNull
                                                  Just time -> Right $ HiValueTime time)
  HiFunRand           -> applyRandAct
  HiFunEcho           -> applyUnAct HiActionEcho
----- Dictionary functions -----
  HiFunKeys           -> applyMapFunc (HiValueList . DS.fromList . DM.keys)
  HiFunValues         -> applyMapFunc (HiValueList . DS.fromList . DM.elems)
  HiFunCount          -> applyCount
  HiFunInvert         -> applyMapFunc
    (HiValueDict .
    DM.fromList .
    map (\lst -> ((fst . head) lst, (HiValueList . DS.fromList) $ map snd lst)) .
    groupBy ((==) `on` fst) .
    sortBy (\(a, _) (b,_ ) -> compare a b) .
    map swap .
    DM.assocs)

-- | Maps functions that have overloads for different types of arguments to their implementation.
applyDualFunc :: HiFun -> [HiValue] -> Either HiError HiValue
----- Addition -----
applyDualFunc HiFunAdd [HiValueString a, HiValueString b] = Right $ HiValueString $ DT.append a b
applyDualFunc HiFunAdd [HiValueList a, HiValueList b]     = Right $ HiValueList $ a >< b
applyDualFunc HiFunAdd [HiValueBytes a, HiValueBytes b]   = Right $ HiValueBytes $ DBS.append a b
applyDualFunc HiFunAdd [HiValueTime a, HiValueNumber b]   = Right $ HiValueTime $
                                                            addUTCTime (fromRational b) a
applyDualFunc HiFunAdd args                               = applyBiRatioFunc (\a b ->
                                                            Right $ HiValueNumber $ a + b) args
----- Subtraction -----
applyDualFunc HiFunSub [HiValueTime a, HiValueTime b]     = Right $ HiValueNumber $ toRational $
                                                            diffUTCTime a b
applyDualFunc HiFunSub args                               = applyBiRatioFunc (\a b ->
                                                            Right $ HiValueNumber $ a - b) args
----- Multiplication -----
applyDualFunc HiFunMul [HiValueString a, HiValueNumber b] = stimesHelper HiValueString a b
applyDualFunc HiFunMul [HiValueList a, HiValueNumber b]   = stimesHelper HiValueList a b
applyDualFunc HiFunMul [HiValueBytes a, HiValueNumber b]  = stimesHelper HiValueBytes a b
applyDualFunc HiFunMul args                               = applyBiRatioFunc (\a b ->
                                                            Right $ HiValueNumber $ a * b) args
----- Division -----
applyDualFunc HiFunDiv [HiValueString a, HiValueString b] = Right $ HiValueString $
                                                            DT.append (DT.snoc a '/') b
applyDualFunc HiFunDiv args                               = applyBiRatioFunc (\a b ->
                                                            if b /= 0
                                                              then Right $ HiValueNumber $ a / b
                                                              else Left HiErrorDivideByZero) args
----- Length -----
applyDualFunc HiFunLength [HiValueList lst]  = Right $ HiValueNumber $ toRational $ DS.length lst
applyDualFunc HiFunLength [HiValueBytes str] = Right $ HiValueNumber $ toRational $ DBS.length str
applyDualFunc HiFunLength args               = applyStrFunc (Right . HiValueNumber . toRational . DT.length) args
----- Reverse -----
applyDualFunc HiFunReverse [HiValueList lst]  = Right $ HiValueList $ DS.reverse lst
applyDualFunc HiFunReverse [HiValueBytes str] = Right $ HiValueBytes $ DBS.reverse str
applyDualFunc HiFunReverse args               = applyStrFunc (Right . HiValueString . DT.reverse) args
----- Impossible -----
applyDualFunc _ _ = Left HiErrorInvalidFunction

----------------------- ARITHMETIC ------------------------

applyBiRatioFunc
  :: (Rational -> Rational -> Either HiError HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyBiRatioFunc func [HiValueNumber a, HiValueNumber b] = func a b
applyBiRatioFunc _ [_, _]                                = Left HiErrorInvalidArgument
applyBiRatioFunc _ _                                     = Left HiErrorArityMismatch

------------------- BOOLS AND COMPARES --------------------

-- | Executes strict "if" function.
applyIfFunc
  :: (Bool -> HiValue -> HiValue -> Either HiError HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyIfFunc func [HiValueBool cond, true, false] = func cond true false
applyIfFunc _ [_, _, _]                          = Left HiErrorInvalidArgument
applyIfFunc _ _                                  = Left HiErrorArityMismatch

-- | Executes unary bool function, e.g. "not".
applyUnaryBoolFunc
  :: (Bool -> Either HiError HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyUnaryBoolFunc func [HiValueBool a] = func a
applyUnaryBoolFunc _ [_]                = Left HiErrorInvalidArgument
applyUnaryBoolFunc _ _                  = Left HiErrorArityMismatch

-- | Compare two HiValues.
applyBiCompFunc
  :: (HiValue -> HiValue -> Either HiError HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyBiCompFunc func [a, b] = func a b
applyBiCompFunc _ _         = Left HiErrorArityMismatch

-- | Executes binary bool function, e.g. "and"
applyBiBoolFunc
  :: (HiValue -> Bool)
  -> [HiValue]
  -> Either HiError HiValue
applyBiBoolFunc check [a, b] = if check a then Right a else Right b
applyBiBoolFunc _ _          = Left HiErrorArityMismatch

-------------------- STRING FUNCTIONS ---------------------

-- | Executes given function on string.
applyStrFunc
  :: (Text -> Either HiError HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyStrFunc func [HiValueString str] = func str
applyStrFunc _ [_]                    = Left HiErrorInvalidArgument
applyStrFunc _ _                      = Left HiErrorArityMismatch

-------------------- LIST FUNCTIONS ---------------------

-- | Executes fold on list by given binary function.
-- Fold on empty list returns null. Fold on list of one element returns this element.
applyFold :: [HiValue] -> Either HiError HiValue
applyFold [_, HiValueList Empty]                          = Right HiValueNull
applyFold [_, HiValueList (val :<| Empty)]                = Right val
applyFold [HiValueFunction fun, HiValueList (a :<| args)] = foldl foldFunc (Right a) args
  where
    foldFunc :: Either HiError HiValue -> HiValue -> Either HiError HiValue
    foldFunc prevRes currEl = prevRes >>= applyHelper
      where
        applyHelper :: HiValue -> Either HiError HiValue
        applyHelper arg = applyFunc fun [arg, currEl]
applyFold [_, HiValueList _]                              = Left HiErrorInvalidArgument
applyFold _                                               = Left HiErrorArityMismatch

-- | Wrap list of arguments into HiValueList.
applyList :: [HiValue] -> Either HiError HiValue
applyList lst = Right $ HiValueList $ DS.fromList lst

------------------ BYTESTRING FUNCTIONS -------------------

-- | Serialises fiven element.
applySerialise :: [HiValue] -> Either HiError HiValue
applySerialise [val] = Right $ HiValueBytes $ DBSL.toStrict $ serialise val
applySerialise _     = Left HiErrorArityMismatch

-- | Converts string to UTF-8 encoded bytestring.
applyEncode :: [HiValue] -> Either HiError HiValue
applyEncode [HiValueString str] = Right $ HiValueBytes $ encodeUtf8 str
applyEncode [_]                 = Left HiErrorInvalidArgument
applyEncode _                   = Left HiErrorArityMismatch

-- | Packs list of numbers from 0 to 255 into a bytestring.
applyPack :: [HiValue] -> Either HiError HiValue
applyPack [HiValueList lst] = HiValueBytes . DBS.pack . toList <$> mapM (\case
  HiValueNumber int ->
    if isInteger int && (int >= 0) && (int <= 255)
      then Right $ (fromInteger :: Integer -> Word8) $ numerator int
      else Left HiErrorInvalidArgument
  _ -> Left HiErrorInvalidArgument) lst
applyPack [_]               = Left HiErrorInvalidArgument
applyPack _                 = Left HiErrorArityMismatch

-- | Executes given function on bytestring.
applyByteFunc
  :: (DBS.ByteString -> Either HiError HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyByteFunc func [HiValueBytes str] = func str
applyByteFunc _ [_]                   = Left HiErrorInvalidArgument
applyByteFunc _ _                     = Left HiErrorArityMismatch

------------------------- ACTIONS -------------------------

-- | Converts "write" function into "write" action.
applyWriteAct
  :: (FilePath -> DBS.ByteString -> HiAction)
  -> [HiValue]
  -> Either HiError HiValue
applyWriteAct constr [HiValueString pth, HiValueString str] = Right $ HiValueAction $ constr (DT.unpack pth) (encodeUtf8 str)
applyWriteAct constr [HiValueString pth, HiValueBytes str]  = Right $ HiValueAction $ constr (DT.unpack pth) str
applyWriteAct _ [_, _]                                      = Left HiErrorInvalidArgument
applyWriteAct _ _                                           = Left HiErrorArityMismatch

-- | Converts given function into correspong action.
applyUnAct :: (Text -> HiAction) -> [HiValue] -> Either HiError HiValue
applyUnAct constr [HiValueString str] = Right $ HiValueAction $ constr str
applyUnAct _ [_]                      = Left HiErrorInvalidArgument
applyUnAct _ _                        = Left HiErrorArityMismatch

-- | Converts "rand" function into "rand" action.
-- Checks, if it was given correct numbers (so that they are in Int bound).
applyRandAct :: [HiValue] -> Either HiError HiValue
applyRandAct [HiValueNumber a, HiValueNumber b] = if isInteger a && isInteger b && validateInt a && validateInt b
                                                    then Right $ HiValueAction $ HiActionRand (fromIntegral $ numerator a) (fromIntegral $ numerator b)
                                                    else Left HiErrorInvalidArgument
applyRandAct [_, _]                             = Left HiErrorInvalidArgument
applyRandAct _                                  = Left HiErrorArityMismatch

------------------ DICTIONARY FUNCTIONS -------------------

-- | Execute "count" function.
applyCount :: [HiValue] -> Either HiError HiValue
applyCount [val] =
    case val of
    HiValueString txt -> Right $ countHelper (HiValueString . DT.singleton) (DT.unpack txt)
    HiValueList lst   -> Right $ countHelper id (toList lst)
    HiValueBytes str  -> Right $ countHelper (HiValueNumber . toRational) (DBS.unpack str)
    _                 -> Left HiErrorInvalidArgument
applyCount _                   = Left HiErrorArityMismatch

-- | Transforms list of values into dictionary.
-- Keys are different elements of the list.
-- Values are number of occurences of corresponding element.
countHelper :: (Eq a, Ord a) => (a -> HiValue) -> [a] -> HiValue
countHelper constructor = HiValueDict . DM.fromList .
  map (\vals -> ((constructor . head) vals, (HiValueNumber . toRational . length) vals)) .
  group . sort

-- | Executes given function on dictionary.
applyMapFunc
  :: (DM.Map HiValue HiValue -> HiValue)
  -> [HiValue]
  -> Either HiError HiValue
applyMapFunc func [HiValueDict m] = Right $ func m
applyMapFunc _ [_]                = Left HiErrorInvalidArgument
applyMapFunc _ _                  = Left HiErrorArityMismatch

------------------------- HELPERS -------------------------

-- | Generalized function for indexing or slicing depending on number of arguments.
indexAll
  :: t a                     -- ^ Collection to take element or slice from
  -> (t a -> Int)            -- ^ Function, counting length of the collection
  -> (t a -> Maybe a)        -- ^ Function, safely taking first element
  -> (Int -> t a -> t a)     -- ^ Function, taking first n elements
  -> (Int -> t a -> t a)     -- ^ Function, dropping last n elements
  -> (t a -> HiValue)        -- ^ Constructor of HiValue from slice
  -> (a -> HiValue)          -- ^ Constructor of Hivalue from single element
  -> [HiValue]               -- ^ List of arguments
  -> Either HiError HiValue  -- ^ Function result
indexAll obj len maybeFirst takeFun dropFun constructor wrapper args =
  case args of
    [HiValueNumber i]                  -> getByIdAll obj maybeFirst takeFun dropFun wrapper i
    [HiValueNumber l, HiValueNumber r] -> getSliceAll obj len takeFun dropFun constructor l r
    [HiValueNumber l, HiValueNull]     -> getSliceAll obj len takeFun dropFun constructor l (toRational $ len obj)
    [HiValueNull, HiValueNumber r]     -> getSliceAll obj len takeFun dropFun constructor 0 r
    [HiValueNull, HiValueNull]         -> getSliceAll obj len takeFun dropFun constructor 0 (toRational $ len obj)
    [_]                                -> Left HiErrorInvalidArgument
    [_, _]                             -> Left HiErrorInvalidArgument
    _                                  -> Left HiErrorArityMismatch

-- | Generalized indexing function.
getByIdAll
  :: t a                     -- ^ Collection to take element from
  -> (t a -> Maybe a)        -- ^ Function, safely taking first element
  -> (Int -> t a -> t a)     -- ^ Function, taking first n elements
  -> (Int -> t a -> t a)     -- ^ Function, dropping last n elements
  -> (a -> HiValue)          -- ^ Constructor of Hivalue from single element
  -> Rational                -- ^ Index
  -> Either HiError HiValue  -- ^ Function result
getByIdAll obj getFirst takeFun dropFun wrapper i =
  if isInteger i
  then let
    idx = fromIntegral $ numerator i
    first = getFirst $ (dropFun idx . takeFun (idx + 1)) obj in
    case first of
      Nothing -> Right HiValueNull
      Just e  -> Right $ wrapper e
  else Left HiErrorInvalidArgument

-- | Generalized slicing function.
getSliceAll
  :: t a                     -- ^ Collection to take element from
  -> (t a -> Int)            -- ^ Function, counting length of the collection
  -> (Int -> t a -> t a)     -- ^ Function, taking first n elements
  -> (Int -> t a -> t a)     -- ^ Function, dropping last n elements
  -> (t a -> HiValue)        -- ^ Constructor of HiValue from slice
  -> Rational                -- ^ Left border
  -> Rational                -- ^ Right border
  -> Either HiError HiValue  -- ^ Function result
getSliceAll lst lenFun takeFun dropFun constructor l r  =
  let left  = checkIdx l
      right = checkIdx r
  in case (left, right) of
    (Just trueLeft, Just trueRight) -> Right $ constructor $ dropFun trueLeft $ takeFun trueRight lst
    (_, _) -> Left HiErrorInvalidArgument
  where
    checkIdx :: Rational -> Maybe Int
    checkIdx n = if isInteger n then (if n >= 0 then convertPositive n else convertNegative n) else Nothing

    convertPositive :: Rational -> Maybe Int
    convertPositive n = if validateInt n then (Just . fromIntegral . numerator) n else Nothing

    convertNegative :: Rational -> Maybe Int
    convertNegative n = if validateInt n
      then Just $
        let shifted = (fromIntegral . numerator) n + lenFun lst
        in max shifted 0
      else Nothing

-- | Check, if given Rational is in Int bound.
validateInt :: Rational -> Bool
validateInt test = toRational (minBound :: Int) <= test && test <= toRational (maxBound :: Int)

-- | Generalized multiplication of collections, implemented via stimes.
stimesHelper
  :: Semigroup a
  => (a -> HiValue)          -- ^ Constructor of HiValue from given collection
  -> a                       -- ^ Collection to multimply
  -> Rational                -- ^ Number of times the collection should be multiplied
  -> Either HiError HiValue  -- ^ Function result
stimesHelper constructor a b = if isInteger b && b > 0
                                then Right $ constructor $ stimes (numerator b) a
                                else Left HiErrorInvalidArgument

-- | Checks, if given Rational is not a fraction.
isInteger :: Rational -> Bool
isInteger x = denominator x == 1





