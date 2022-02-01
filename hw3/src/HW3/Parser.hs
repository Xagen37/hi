module HW3.Parser
  ( parse
  ) where

import Control.Applicative (Alternative (empty))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as Data.Bytestring
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.List (intercalate)
import qualified Data.Text
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (MonadParsec (eof, notFollowedBy, try), ParseErrorBundle, Parsec, between,
                        choice, many, manyTill, satisfy, sepBy, sepBy1, (<|>))
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))

type Parser = Parsec Void String

-- | Parses HiExpr from String.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = MP.parse (between skipSpace eof globalTerm) ""

-- | Parses one term.
globalTerm :: Parser HiExpr
globalTerm = lexeme $ makeExprParser term operatorTable

-- | Parses one term without infix operators,
-- i.e. expression, or list, or dictionary, or globalTerm in parenthesis,
-- followed by several (maybe 0) dots, exclamation marks or arguments in parenthesis
term :: Parser HiExpr
term = lexeme $ do
  first <- expr <|> hiList <|> dict <|> parenthesis globalTerm
  vals  <- many (runs <|> dots <|> Just <$> args)
  return $ folder first vals
    where
      -- | Wraps arguments and dot arguments into HiExprApply,
      -- and wraps exclamations into HiExprRun.
      folder :: HiExpr -> [Maybe [HiExpr]] -> HiExpr
      folder acc lst = case lst of
        []                 -> acc
        Nothing : next     -> folder (HiExprRun acc) next
        Just values : next -> folder (HiExprApply acc values) next

-- | Parses list, i.e. several (maybe 0) terms in square parenthesis, divided by commas.
hiList :: Parser HiExpr
hiList = lexeme $ do
  inner <- between (lexeme $ char '[') (char ']') (sepBy globalTerm (lexeme $ char ','))
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) inner

-- | Parses dictionary, i.e. several (maybe 0) pairs in curly parenthesis, divided by commas.
-- Pair is two globalTerms divided by colon.
dict :: Parser HiExpr
dict = lexeme $ do
  pairs <- between (lexeme $ char '{') (char '}') (sepBy pair (lexeme $ char ','))
  return $ HiExprDict pairs

-- | Parses dictionary pair, i.e. two globalTerms fivided by colon.
pair :: Parser (HiExpr, HiExpr)
pair = lexeme $ do
  key <- globalTerm
  _ <- lexeme $ char ':'
  value <- globalTerm
  return (key, value)

-- | Parses function arguments,
-- i.e. several (maybe 0) globalTerms in parenthesis, divided by commas.
args :: Parser [HiExpr]
args = lexeme $ parenthesis $ sepBy globalTerm (lexeme $ char ',')

-- | Parses exclamation marks.
runs :: Parser (Maybe [HiExpr])
runs = lexeme $ Nothing <$ char '!'

-- | Parses dot arguments, i.e. dot and then string identificator.
dots :: Parser (Maybe [HiExpr])
dots = lexeme $ do
  _ <- char '.'
  key <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  return $ Just [HiExprValue $ HiValueString $ Data.Text.pack $ intercalate "-" key]

-- | Parses expression, i.e. any HiValue except list or dictionary.
expr :: Parser HiExpr
expr = lexeme $ HiExprValue <$> val

-- | Tries to run HiValue parser and wraps its result into corresponding constructor.
val :: Parser HiValue
val = choice
  [ HiValueNumber <$> numeric
  , HiValueFunction <$> fun
  , HiValueBool <$> bool
  , hiNull
  , HiValueString . Data.Text.pack <$> str
  , HiValueBytes <$> bytes
  , HiValueAction <$> cwd
  , HiValueAction <$> now
  ]

-- | Parses number.
numeric :: Parser Rational
numeric = lexeme $ toRational <$> L.signed skipSpace L.scientific

-- | Parses function.
fun :: Parser HiFun
fun = lexeme $ choice
  [ HiFunAdd            <$ string "add"
  , HiFunDiv            <$ string "div"
  , HiFunMul            <$ string "mul"
  , HiFunSub            <$ string "sub"
  , HiFunAnd            <$ string "and"
  , HiFunOr             <$ string "or"
  , HiFunLessThan       <$ string "less-than"
  , HiFunGreaterThan    <$ string "greater-than"
  , HiFunEquals         <$ string "equals"
  , HiFunNotLessThan    <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals      <$ string "not-equals"
  , HiFunIf             <$ string "if"
  , HiFunNot            <$ string "not"
  , HiFunLength         <$ string "length"
  , HiFunToUpper        <$ string "to-upper"
  , HiFunToLower        <$ string "to-lower"
  , HiFunReverse        <$ string "reverse"
  , HiFunTrim           <$ string "trim"
  , HiFunList           <$ string "list"
  , HiFunRange          <$ string "range"
  , HiFunFold           <$ string "fold"
  , HiFunPackBytes      <$ string "pack-bytes"
  , HiFunUnpackBytes    <$ string "unpack-bytes"
  , HiFunEncodeUtf8     <$ string "encode-utf8"
  , HiFunDecodeUtf8     <$ string "decode-utf8"
  , HiFunZip            <$ string "zip"
  , HiFunUnzip          <$ string "unzip"
  , HiFunSerialise      <$ string "serialise"
  , HiFunDeserialise    <$ string "deserialise"
  , HiFunRead           <$ string "read"
  , HiFunWrite          <$ string "write"
  , HiFunMkDir          <$ string "mkdir"
  , HiFunChDir          <$ string "cd"
  , HiFunParseTime      <$ string "parse-time"
  , HiFunRand           <$ string "rand"
  , HiFunEcho           <$ string "echo"
  , HiFunCount          <$ string "count"
  , HiFunKeys           <$ string "keys"
  , HiFunValues         <$ string "values"
  , HiFunInvert         <$ string "invert"
  ]

-- | Parses bool value.
bool :: Parser Bool
bool = lexeme $ choice
  [ False <$ string "false"
  , True  <$ string "true"
  ]

-- | Parses null into HiValueNull.
hiNull :: Parser HiValue
hiNull = lexeme $ HiValueNull <$ string "null"

-- | Parses string.
str :: Parser String
str = lexeme $ char '"' *> manyTill L.charLiteral  (char '"')

-- | Parses bytestring, i.e. several (maybe 0) surrounded by "[#" and "#]"
-- pairs of hexadecimal digits divided by spaces
bytes :: Parser ByteString
bytes = lexeme $ do
  inner <- between (lexeme $ string "[#") (string "#]") (many byte)
  return $ Data.Bytestring.pack inner

-- | Parses one byte of bytestring, i.e. exactly two hexadecimal digits.
byte :: Parser Word8
byte = lexeme $ do
  a <- toEnum . digitToInt <$> hexDigitChar
  b <- toEnum . digitToInt <$> hexDigitChar <* notFollowedBy hexDigitChar
  return $ a * 16 + b

-- | Parses action "cwd" (current working directory).
cwd :: Parser HiAction
cwd = lexeme $ HiActionCwd <$ string "cwd"

-- | Parses action "now" (return current time).
now :: Parser HiAction
now = lexeme $ HiActionNow <$ string "now"

--------------------- HELPERS -----------------------------

-- | Parser, consuming whitespace characters.
skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

-- | Skips all whitespaces after the next parser.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

-- | Runs parser surrounded by parenthesis.
parenthesis :: Parser a -> Parser a
parenthesis = between (lexeme $ char '(') (char ')')

-- | Table of operators for makeExprParser.
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryL "*" HiFunMul
    , divL    "/" HiFunDiv
    ]
  , [ binaryL "+" HiFunAdd
    , binaryL "-" HiFunSub
    ]
  , [ binaryN "<=" HiFunNotGreaterThan
    , binaryN ">=" HiFunNotLessThan
    , binaryN "<" HiFunLessThan
    , binaryN ">" HiFunGreaterThan
    , binaryN "==" HiFunEquals
    , binaryN "/=" HiFunNotEquals
    ]
  , [ binaryR "&&" HiFunAnd]
  , [ binaryR "||" HiFunOr ]
  ]

-- | Constructor for parser of division operator.
-- It is separated because of conflict with "/=" operator.
divL :: String -> HiFun -> Operator Parser HiExpr
divL name f = InfixL $ lexeme $ try $ hiBiExprApplier f <$ string name <* notFollowedBy (char '=')

-- | Constructor for parser of infix binary operator with left associativity.
binaryL :: String -> HiFun -> Operator Parser HiExpr
binaryL name f = InfixL $ binary name f

-- | Constructor for parser of infix binary operator with no associativity.
binaryN :: String -> HiFun -> Operator Parser HiExpr
binaryN name f = InfixN $ binary name f

-- | Constructor for parser of infix binary operator with right associativity.
binaryR :: String -> HiFun -> Operator Parser HiExpr
binaryR name f = InfixR $ binary name f

-- | Helper for constructor for parser of binary operator.
binary :: String -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
binary name f = lexeme $ hiBiExprApplier f <$ string name

-- | Helper for constructor for parser of binary operator.
-- Takes binary HiFun and its two arguments, then wrap them into HiExprApply.
hiBiExprApplier :: HiFun -> HiExpr -> HiExpr -> HiExpr
hiBiExprApplier f a b = HiExprApply (HiExprValue $ HiValueFunction f) [a, b]
