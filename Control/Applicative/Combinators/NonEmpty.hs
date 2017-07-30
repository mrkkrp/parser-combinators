-- |
-- Module      :  Control.Applicative.Combinators
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
<<<<<<< HEAD
-- This module provides non-empty list variants of some of the functions in
-- "Control.Applicative.Combinators" and re-exports `some1` from
-- "Data.List.NonEmpty".
=======
-- This module provides 'NonEmpty' list variants of some of the functions in
-- "Control.Applicative.Combinators".
>>>>>>> df37cd900d251637c07de4e2b0e8455fb54816c5
--
-- @since 0.2.0

module Control.Applicative.Combinators.NonEmpty
<<<<<<< HEAD
  ( endBy1
  , sepBy1
  , sepEndBy1
  , NE.some1
  , someTill
  )
where

import Control.Applicative
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import qualified Control.Applicative.Combinators as C
=======
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
>>>>>>> df37cd900d251637c07de4e2b0e8455fb54816c5

-- | @'endBy1' p sep@ parses /one/ or more occurrences of @p@, separated and
-- ended by @sep@. Returns a non-empty list of values returned by @p@.

endBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
endBy1 p sep = NE.fromList <$> C.endBy1 p sep
{-# INLINE endBy1 #-}

<<<<<<< HEAD
=======
-- | @'someTill' p end@ works similarly to @'C.manyTill' p end@, but @p@
-- should succeed at least once.
--
-- See also: 'C.skipSome', 'C.skipSomeTill'.

someTill :: Alternative m => m a -> m end -> m (NonEmpty a)
someTill p end = NE.fromList <$> C.someTill p end
{-# INLINE someTill #-}

>>>>>>> df37cd900d251637c07de4e2b0e8455fb54816c5
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
<<<<<<< HEAD

-- | @'someTill' p end@ works similarly to @'C.manyTill' p end@, but @p@
-- should succeed at least once.
--
-- See also: 'C.skipSome', 'C.skipSomeTill'.

someTill :: Alternative m => m a -> m end -> m (NonEmpty a)
someTill p end = NE.fromList <$> C.someTill p end
{-# INLINE someTill #-}
=======
>>>>>>> df37cd900d251637c07de4e2b0e8455fb54816c5
