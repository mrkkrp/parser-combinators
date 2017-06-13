-- |
-- Module      :  Control.Applicative.Combinators
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser combinators defined for instances of 'Applicative' and
-- 'Alternative'. This module also re-exports functions that are commonly
-- used in parsing from "Control.Applicative".

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}

module Control.Applicative.Combinators
  ( (<|>)
    -- $assocbo
  , between
  , choice
  , count
  , count'
  , eitherP
  , endBy
  , endBy1
  , many
    -- $many
  , manyTill
  , some
    -- $some
  , someTill
  , option
  , optional
    -- $optional
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , skipMany
  , skipSome
  , skipManyTill
  , skipSomeTill )
where

import Control.Applicative
import Control.Monad (void)
import Data.Foldable

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (sequenceA)
#endif

-- $assocbo
--
-- This combinator implements choice. The parser @p \<|> q@ first applies
-- @p@. If it succeeds, the value of @p@ is returned. If @p@ fails
-- /without consuming any input/, parser @q@ is tried.
--
-- The parser is called /predictive/ since @q@ is only tried when parser @p@
-- didn't consume any input (i.e. the look ahead is 1). This
-- non-backtracking behaviour allows for both an efficient implementation of
-- the parser combinators and the generation of good error messages.

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- > braces = between (symbol "{") (symbol "}")

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close
{-# INLINE between #-}

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order, until
-- one of them succeeds. Returns the value of the succeeding parser.

choice :: (Foldable f, Alternative m) => f (m a) -> m a
choice = asum
{-# INLINE choice #-}

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or equal
-- to zero, the parser equals to @return []@. Returns a list of @n@ values.
--
-- See also: 'count''.

count :: Applicative m => Int -> m a -> m [a]
count n p = sequenceA (replicate n p)
{-# INLINE count #-}

-- | @count' m n p@ parses from @m@ to @n@ occurrences of @p@. If @n@ is not
-- positive or @m > n@, the parser equals to @return []@. Returns a list of
-- parsed values.
--
-- Please note that @m@ /may/ be negative, in this case effect is the same
-- as if it were equal to zero.
--
-- See also: 'count'.

count' :: Alternative m => Int -> Int -> m a -> m [a]
count' m' n' p = go m' n'
  where
    go !m !n
      | n <= 0 || m > n = pure []
      | m > 0           = (:) <$> p <*> go (m - 1) (n - 1)
      | otherwise       =
          let f t ts = maybe [] (:ts) t
          in f <$> optional p <*> go 0 (pred n)
{-# INLINE count' #-}

-- | Combine two alternatives.

eitherP :: Alternative m => m a -> m b -> m (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a list of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semicolon

endBy :: Alternative m => m a -> m sep -> m [a]
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a list of values returned by @p@.

endBy1 :: Alternative m => m a -> m sep -> m [a]
endBy1 p sep = some (p <* sep)
{-# INLINE endBy1 #-}

-- $many
--
-- @many p@ applies the parser @p@ /zero/ or more times and returns a list
-- of the returned values of @p@. Note that if the @p@ parser fails
-- consuming input, then the entire @many p@ parser fails with the error
-- message @p@ produced instead of just stopping iterating. In these cases
-- wrapping @p@ with 'try' may be desirable.
--
-- > identifier = (:) <$> letter <*> many (alphaNumChar <|> char '_')

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until parser
-- @end@ succeeds. Returns the list of values returned by @p@.
--
-- See also: 'skipMany', 'skipManyTill'.

manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = go where go = ([] <$ end) <|> ((:) <$> p <*> go)
{-# INLINE manyTill #-}

-- $some
--
-- @some p@ applies the parser @p@ /one/ or more times and returns a list of
-- the returned values of @p@. The note about behavior of the combinator in
-- the case when @p@ fails consuming input (see 'A.many') applies to 'some'
-- as well.
--
-- > word = some letter

-- | @someTill p end@ works similarly to @manyTill p end@, but @p@ should
-- succeed at least once.
--
-- See also: 'skipSome', 'skipSomeTill'.

someTill :: Alternative m => m a -> m end -> m [a]
someTill p end = (:) <$> p <*> manyTill p end
{-# INLINE someTill #-}

-- | @option x p@ tries to apply the parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value returned
-- by @p@.
--
-- > priority = option 0 (digitToInt <$> digitChar)
--
-- See also: 'optional'.

option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x
{-# INLINE option #-}

-- $optional
--
-- @optional p@ tries to apply the parser @p@. It will parse @p@ or nothing.
-- It only fails if @p@ fails after consuming input. On success result of
-- @p@ is returned inside of 'Just', on failure 'Nothing' is returned.

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
{-# INLINE sepBy1 #-}

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []
{-# INLINE sepEndBy #-}

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping its
-- result.
--
-- See also: 'manyTill', 'skipManyTill'.

skipMany :: Alternative m => m a -> m ()
skipMany p = void $ many p
{-# INLINE skipMany #-}

-- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping its
-- result.
--
-- See also: 'someTill', 'skipSomeTill'.

skipSome :: Alternative m => m a -> m ()
skipSome p = void $ some p
{-# INLINE skipSome #-}

-- | @skipManyTill p end@ applies the parser @p@ /zero/ or more times
-- skipping results until parser @end@ succeeds. Result parsed by @end@ is
-- then returned.
--
-- See also: 'manyTill', 'skipMany'.

skipManyTill :: Alternative m => m a -> m end -> m end
skipManyTill p end = go
  where
    go = end <|> (p *> go)
{-# INLINE skipManyTill #-}

-- | @skipSomeTill p end@ applies the parser @p@ /one/ or more times
-- skipping results until parser @end@ succeeds. Result parsed by @end@ is
-- then returned.
--
-- See also: 'someTill', 'skipSome'.

skipSomeTill :: Alternative m => m a -> m end -> m end
skipSomeTill p end = p *> skipManyTill p end
{-# INLINE skipSomeTill #-}
