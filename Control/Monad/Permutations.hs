-- |
-- Module      :  Control.Monad.Permutations
-- Copyright   :  © 2017–present Alex Washburn
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module specialized the interface to 'Monad' for potential efficiency
-- considerations, depending on the monad the permutations are run over.
--
-- For a more general interface requiring only 'Applicative', and for more
-- complete documentation, see the "Control.Applicative.Permutations" module.
--
-- @since 1.3.0
module Control.Monad.Permutations
  ( -- ** Permutation type
    Permutation,

    -- ** Permutation evaluators
    runPermutation,
    intercalateEffect,

    -- ** Permutation constructors
    toPermutation,
    toPermutationWithDefault,
  )
where

import Control.Applicative

-- | An 'Applicative' wrapper-type for constructing permutation parsers.
data Permutation m a = P !(Maybe a) (m (Permutation m a))

instance (Functor m) => Functor (Permutation m) where
  fmap f (P v p) = P (f <$> v) (fmap f <$> p)

instance (Alternative m) => Applicative (Permutation m) where
  pure value = P (Just value) empty
  lhs@(P f v) <*> rhs@(P g w) = P (f <*> g) (lhsAlt <|> rhsAlt)
    where
      lhsAlt = (<*> rhs) <$> v
      rhsAlt = (lhs <*>) <$> w
  liftA2 f lhs@(P x v) rhs@(P y w) = P (liftA2 f x y) (lhsAlt <|> rhsAlt)
    where
      lhsAlt = (\p -> liftA2 f p rhs) <$> v
      rhsAlt = liftA2 f lhs <$> w

-- | \"Unlifts\" a permutation parser into a parser to be evaluated.
runPermutation ::
  ( Alternative m,
    Monad m
  ) =>
  -- | Permutation specification
  Permutation m a ->
  -- | Resulting base monad capable of handling the permutation
  m a
runPermutation (P value parser) = optional parser >>= f
  where
    f Nothing = maybe empty pure value
    f (Just p) = runPermutation p

-- | \"Unlifts\" a permutation parser into a parser to be evaluated with an
-- intercalated effect. Useful for separators between permutation elements.
intercalateEffect ::
  ( Alternative m,
    Monad m
  ) =>
  -- | Effect to be intercalated between permutation components
  m b ->
  -- | Permutation specification
  Permutation m a ->
  -- | Resulting base monad capable of handling the permutation
  m a
intercalateEffect = run noEffect
  where
    noEffect = pure ()
    run :: (Alternative m, Monad m) => m c -> m b -> Permutation m a -> m a
    run headSep tailSep (P value parser) = optional (headSep *> parser) >>= f
      where
        f Nothing = maybe empty pure value
        f (Just p) = run tailSep tailSep p

-- | \"Lifts\" a parser to a permutation parser.
toPermutation ::
  (Alternative m) =>
  -- | Permutation component
  m a ->
  Permutation m a
toPermutation p = P Nothing $ pure <$> p

-- | \"Lifts\" a parser with a default value to a permutation parser.
--
-- If no permutation containing the supplied parser can be parsed from the input,
-- then the supplied default value is returned in lieu of a parse result.
toPermutationWithDefault ::
  (Alternative m) =>
  -- | Default Value
  a ->
  -- | Permutation component
  m a ->
  Permutation m a
toPermutationWithDefault v p = P (Just v) $ pure <$> p
