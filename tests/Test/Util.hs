module Test.Util
  ( Parser
  , prs
  , prs_
  , abcRow
  , rightOrder
  , g )
where

import Data.Void
import Test.Hspec (Spec, it)
import Test.Hspec.Megaparsec (shouldParse)
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

-- | Check that the given parser returns the list in the right order.

rightOrder
  :: Parser String     -- ^ The parser to test
  -> String            -- ^ Input for the parser
  -> String            -- ^ Expected result
  -> Spec
rightOrder p s s' =
  it "produces the list in the right order" $
    prs_ p s `shouldParse` s'

g :: Int -> Int
g x = x + if x > 0 then x - 1 else 0
