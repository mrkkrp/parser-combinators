module Test.Util
  ( Parser
  , prs
  , prs_
  , abcRow )
where

import Data.Void
import Text.Megaparsec

-- | The type of parser that consumes a 'String'.

type Parser = Parsec Void String

-- | Apply parser to given input. This is a specialized version of 'parse'
-- that assumes empty file name.

prs
  :: Parser a          -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> Either (ParseError Char Void) a -- ^ Result of parsing
prs p = parse p ""

-- | Just like 'prs', but forces the parser to consume all input by adding
-- 'eof':
--
-- > prs_ p = parse (p <* eof) ""

prs_
  :: Parser a          -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> Either (ParseError Char Void) a -- ^ Result of parsing
prs_ p = parse (p <* eof) ""

-- | @abcRow a b c@ generates string consisting of character “a” repeated
-- @a@ times, character “b” repeated @b@ times, and character “c” repeated
-- @c@ times.

abcRow :: Int -> Int -> Int -> String
abcRow a b c = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'
