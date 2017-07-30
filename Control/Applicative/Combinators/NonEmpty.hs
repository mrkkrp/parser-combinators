-- |
-- Module      :  Control.Applicative.Combinators
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides 'NonEmpty' list variants of some of the functions in
-- "Control.Applicative.Combinators".
--
-- @since 0.2.0

module Control.Applicative.Combinators.NonEmpty
  ( some
  , endBy1
  , someTill
  , sepBy1
  , sepEndBy1 )
where

import Control.Applicative hiding (some)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Control.Applicative.Combinators as C
import qualified Data.List.NonEmpty              as NE

-- | @'some' p@ applies the parser @p@ /one/ or more times and returns a
-- list of the returned values of @p@.
--
-- > word = some letter

some :: Alternative m => m a -> m (NonEmpty a)
some p = NE.fromList <$> C.some p
{-# INLINE some #-}

-- | @'endBy1' p sep@ parses /one/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a non-empty list of values returned by @p@.

endBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
endBy1 p sep = NE.fromList <$> C.endBy1 p sep
{-# INLINE endBy1 #-}

-- | @'someTill' p end@ works similarly to @'C.manyTill' p end@, but @p@
-- should succeed at least once.
--
-- See also: 'C.skipSome', 'C.skipSomeTill'.

someTill :: Alternative m => m a -> m end -> m (NonEmpty a)
someTill p end = NE.fromList <$> C.someTill p end
{-# INLINE someTill #-}

-- | @'sepBy1' p sep@ parses /one/ or more occurrences of @p@, separated by
-- @sep@. Returns a non-empty list of values returned by @p@.

sepBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1 p sep = NE.fromList <$> C.sepBy1 p sep
{-# INLINE sepBy1 #-}

-- | @'sepEndBy1' p sep@ parses /one/ or more occurrences of @p@, separated
-- and optionally ended by @sep@. Returns a non-empty list of values returned by
-- @p@.

sepEndBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndBy1 p sep = NE.fromList <$> C.sepEndBy1 p sep
{-# INLINE sepEndBy1 #-}
