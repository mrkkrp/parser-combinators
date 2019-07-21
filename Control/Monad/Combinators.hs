-- |
-- Module      :  Control.Monad.Combinators
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides more efficient versions of the combinators from
-- "Control.Applicative.Combinators" defined in terms of 'Monad' and
-- 'MonadPlus' instead of 'Control.Applicative.Applicative' and
-- 'Control.Applicative.Alternative'. When there is no difference in
-- performance we just re-export the combinators from
-- "Control.Applicative.Combinators".
--
-- @since 0.4.0

{-# LANGUAGE BangPatterns #-}

module Control.Monad.Combinators
  ( -- * Re-exports from "Control.Applicative"
    (C.<|>)
    -- $assocbo
  , C.optional
    -- $optional
  , C.empty
    -- $empty

    -- * Original combinators
  , C.between
  , C.choice
  , count
  , count'
  , C.eitherP
  , endBy
  , endBy1
  , many
  , manyTill
  , manyTill_
  , some
  , someTill
  , someTill_
  , C.option
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , skipMany
  , skipSome
  , skipCount
  , skipManyTill
  , skipSomeTill )
where

import Control.Monad
import qualified Control.Applicative.Combinators as C

----------------------------------------------------------------------------
-- Re-exports from "Control.Applicative"

-- $assocbo
--
-- This combinator implements choice. The parser @p 'C.<|>' q@ first applies
-- @p@. If it succeeds, the value of @p@ is returned. If @p@ fails, parser
-- @q@ is tried.

-- $optional
--
-- @'C.optional' p@ tries to apply the parser @p@. It will parse @p@ or
-- 'Nothing'. It only fails if @p@ fails after consuming input. On success
-- result of @p@ is returned inside of 'Just', on failure 'Nothing' is
-- returned.
--
-- See also: 'C.option'.

-- $empty
--
-- This parser fails unconditionally without providing any information about
-- the cause of the failure.

----------------------------------------------------------------------------
-- Original combinators

-- | @'count' n p@ parses @n@ occurrences of @p@. If @n@ is smaller or equal
-- to zero, the parser equals to @'return' []@. Returns a list of @n@
-- values.
--
-- See also: 'skipCount', 'count''.

count :: Monad m => Int -> m a -> m [a]
count n' p = go id n'
  where
    go f !n =
      if n <= 0
        then return (f [])
        else do
          x <- p
          go (f . (x:)) (n - 1)
{-# INLINE count #-}

-- | @'count'' m n p@ parses from @m@ to @n@ occurrences of @p@. If @n@ is
-- not positive or @m > n@, the parser equals to @'return' []@. Returns a
-- list of parsed values.
--
-- Please note that @m@ /may/ be negative, in this case effect is the same
-- as if it were equal to zero.
--
-- See also: 'skipCount', 'count'.

count' :: MonadPlus m => Int -> Int -> m a -> m [a]
count' m' n' p =
  if n' > 0 && n' >= m'
    then gom id m'
    else return []
  where
    gom f !m =
      if m > 0
        then do
          x <- p
          gom (f . (x:)) (m - 1)
        else god f (if m' <= 0 then n' else n' - m')
    god f !d =
      if d > 0
        then do
          r <- C.optional p
          case r of
            Nothing -> return (f [])
            Just  x -> god (f . (x:)) (d - 1)
        else return (f [])
{-# INLINE count' #-}

-- | @'endBy' p sep@ parses /zero/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a list of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semicolon

endBy :: MonadPlus m => m a -> m sep -> m [a]
endBy p sep = many (p >>= \x -> x <$ sep)
{-# INLINE endBy #-}

-- | @'endBy1' p sep@ parses /one/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a list of values returned by @p@.

endBy1 :: MonadPlus m => m a -> m sep -> m [a]
endBy1 p sep = some (p >>= \x -> x <$ sep)
{-# INLINE endBy1 #-}

-- | @'many' p@ applies the parser @p@ /zero/ or more times and returns a
-- list of the values returned by @p@.
--
-- > identifier = (:) <$> letter <*> many (alphaNumChar <|> char '_')

many :: MonadPlus m => m a -> m [a]
many p = go id
  where
    go f = do
      r <- C.optional p
      case r of
        Nothing -> return (f [])
        Just  x -> go (f . (x:))
{-# INLINE many #-}

-- | @'manyTill' p end@ applies parser @p@ /zero/ or more times until parser
-- @end@ succeeds. Returns the list of values returned by @p@. __Note__ that
-- @end@ result is consumed and lost. Use 'manyTill_' if you wish to keep
-- it.
--
-- See also: 'skipMany', 'skipManyTill'.

manyTill :: MonadPlus m => m a -> m end -> m [a]
manyTill p end = fst <$> manyTill_ p end
{-# INLINE manyTill #-}

-- | @'manyTill_' p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@ and the
-- @end@ result. Use 'manyTill' if you have no need in the result of the
-- @end@.
--
-- See also: 'skipMany', 'skipManyTill'.
--
-- @since 1.2.0

manyTill_ :: MonadPlus m => m a -> m end -> m ([a], end)
manyTill_ p end = go id
  where
    go f = do
      done <- C.optional end
      case done of
        Just done' -> return (f [], done')
        Nothing  -> do
          x <- p
          go (f . (x:))
{-# INLINE manyTill_ #-}

-- | @'some' p@ applies the parser @p@ /one/ or more times and returns a
-- list of the values returned by @p@.
--
-- > word = some letter

some :: MonadPlus m => m a -> m [a]
some p = liftM2 (:) p (many p)
{-# INLINE some #-}

-- | @'someTill' p end@ works similarly to @'manyTill' p end@, but @p@
-- should succeed at least once. __Note__ that @end@ result is consumed and
-- lost. Use 'someTill_' if you wish to keep it.
--
-- See also: 'skipSome', 'skipSomeTill'.

someTill :: MonadPlus m => m a -> m end -> m [a]
someTill p end = liftM2 (:) p (manyTill p end)
{-# INLINE someTill #-}

-- | @'someTill_' p end@ works similarly to @'manyTill_' p end@, but @p@
-- should succeed at least once. Use 'someTill' if you have no need in the
-- result of the @end@.
--
-- See also: 'skipSome', 'skipSomeTill'.
--
-- @since 1.2.0

someTill_ :: MonadPlus m => m a -> m end -> m ([a], end)
someTill_ p end = liftM2 (\x (xs, y) -> (x:xs, y)) p (manyTill_ p end)
{-# INLINE someTill_ #-}

-- | @'sepBy' p sep@ parses /zero/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma

sepBy :: MonadPlus m => m a -> m sep -> m [a]
sepBy p sep = do
  r <- C.optional p
  case r of
    Nothing -> return []
    Just  x -> (x:) <$> many (sep >> p)
{-# INLINE sepBy #-}

-- | @'sepBy1' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a list of values returned by @p@.

sepBy1 :: MonadPlus m => m a -> m sep -> m [a]
sepBy1 p sep = do
  x <- p
  (x:) <$> many (sep >> p)
{-# INLINE sepBy1 #-}

-- | @'sepEndBy' p sep@ parses /zero/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy :: MonadPlus m => m a -> m sep -> m [a]
sepEndBy p sep = go id
  where
    go f = do
      r <- C.optional p
      case r of
        Nothing -> return (f [])
        Just  x -> do
          more <- C.option False (True <$ sep)
          if more
            then go (f . (x:))
            else return (f [x])
{-# INLINE sepEndBy #-}

-- | @'sepEndBy1' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a list of values returned by @p@.

sepEndBy1 :: MonadPlus m => m a -> m sep -> m [a]
sepEndBy1 p sep = do
  x <- p
  more <- C.option False (True <$ sep)
  if more
    then (x:) <$> sepEndBy p sep
    else return [x]
{-# INLINE sepEndBy1 #-}

-- | @'skipMany' p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- See also: 'manyTill', 'skipManyTill'.

skipMany :: MonadPlus m => m a -> m ()
skipMany p = go
  where
    go = do
      more <- C.option False (True <$ p)
      when more go
{-# INLINE skipMany #-}

-- | @'skipSome' p@ applies the parser @p@ /one/ or more times, skipping its
-- result.
--
-- See also: 'someTill', 'skipSomeTill'.

skipSome :: MonadPlus m => m a -> m ()
skipSome p = p >> skipMany p
{-# INLINE skipSome #-}

-- | @'skipCount' n p@ parses @n@ occurrences of @p@, skipping its result.
-- If @n@ is smaller or equal to zero, the parser equals to @'return' []@.
-- Returns a list of @n@ values.
--
-- See also: 'count', 'count''.

skipCount :: Monad m => Int -> m a -> m ()
skipCount n' p = go n'
  where
    go !n =
      unless (n <= 0) $
        p >> go (n - 1)
{-# INLINE skipCount #-}

-- | @'skipManyTill' p end@ applies the parser @p@ /zero/ or more times
-- skipping results until parser @end@ succeeds. Result parsed by @end@ is
-- then returned.
--
-- See also: 'manyTill', 'skipMany'.

skipManyTill :: MonadPlus m => m a -> m end -> m end
skipManyTill p end = go
  where
    go = do
      r <- C.optional end
      case r of
        Nothing -> p >> go
        Just  x -> return x
{-# INLINE skipManyTill #-}

-- | @'skipSomeTill' p end@ applies the parser @p@ /one/ or more times
-- skipping results until parser @end@ succeeds. Result parsed by @end@ is
-- then returned.
--
-- See also: 'someTill', 'skipSome'.

skipSomeTill :: MonadPlus m => m a -> m end -> m end
skipSomeTill p end = p >> skipManyTill p end
{-# INLINE skipSomeTill #-}
