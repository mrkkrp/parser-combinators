-- |
-- Module      :  Control.Applicative.Permutations
-- Copyright   :  © 2017–2018 Alex Washburn
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is a generalization of the package @parsec-permutation@
-- authored by Samuel Hoffstaetter:
--
-- https://hackage.haskell.org/package/parsec-permutation
--
-- This module also takes inspiration from the algorithm is described in:
-- /Parsing Permutation Phrases/, by Arthur Baars, Andres Löh and Doaitse
-- Swierstra. Published as a functional pearl at the Haskell Workshop 2001:
--
-- https://www.cs.ox.ac.uk/jeremy.gibbons/wg21/meeting56/loeh-paper.pdf
--
-- From these two works we derive a flexible and general method for parsing
-- permutations over an 'Applicative' structure. Quite useful in conjunction
-- with \"Free\" constructions of 'Applicative's, 'Monad's, etc.
--
-- Other permutation parsing libraries tend towards using special \"almost
-- applicative\" combinators for construction which denies the library user
-- the ability to lift and unlift permutation parsing into any 'Applicative'
-- computational context. We redefine these combinators as convenience
-- operators here alongside the equivalent 'Applicative' instance.
--
-- For example, suppose we want to parse a permutation of: an optional
-- string of @a@'s, the character @b@ and an optional @c@. Using a standard
-- parsing library combinator @char@, this can be described using the
-- 'Applicative' instance by:
--
-- > test = runPermutation $
-- >          (,,) <$> toPermutationWithDefault ""  (some (char 'a'))
-- >               <*> toPermutation (char 'b')
-- >               <*> toPermutationWithDefault '_' (char 'c')
--
-- @since 0.2.0

module Control.Applicative.Permutations
  ( -- ** Permutation type
    Permutation
    -- ** Permutation evaluators
  , runPermutation
  , intercalateEffect
    -- ** Permutation constructors
  , toPermutation
  , toPermutationWithDefault )
where

import Control.Applicative

-- | An 'Applicative' wrapper-type for constructing permutation parsers.

data Permutation m a = P (Maybe a) (m (Permutation m a))

instance Functor m => Functor (Permutation m) where
  fmap f (P v p) = P (f <$> v) (fmap f <$> p)

instance Alternative m => Applicative (Permutation m) where
  pure value = P (Just value) empty
  lhs@(P f v) <*> rhs@(P g w) = P (f <*> g) (lhsAlt <|> rhsAlt)
    where
      lhsAlt = (<*> rhs) <$> v
      rhsAlt = (lhs <*>) <$> w

-- | \"Unlifts\" a permutation parser into a parser to be evaluated.

runPermutation
  :: ( Alternative m
     , Monad m)
  => Permutation m a -- ^ Permutation specification
  -> m a             -- ^ Resulting base monad capable of handling the permutation
runPermutation (P value parser) = optional parser >>= f
   where
      f  Nothing = maybe empty pure value
      f (Just p) = runPermutation p

-- | \"Unlifts\" a permutation parser into a parser to be evaluated with an
-- intercalated effect. Useful for separators between permutation elements.
--
-- For example, suppose that similar to above we want to parse a permutation
-- of: an optional string of @a@'s, the character @b@ and an optional @c@.
-- /However/, we also want each element of the permutation to be separated
-- by a colon. Using a standard parsing library combinator @char@, this can
-- be described using the 'Applicative' instance by:
--
-- > test = intercalateEffect (char ':') $
-- >          (,,) <$> toPermutationWithDefault "" (some (char 'a'))
-- >               <*> toPermutation (char 'b')
-- >               <*> toPermutationWithDefault '_' (char 'c')
--
-- This will accept strings such as: \"a:b:c\", \"b:c:a\", \"b:aa\", \"b\",
-- etc.
--
-- Note that the effect is intercalated /between/ permutation components and
-- that:
--
--     * There is never an effect parsed preceeding the first component of
--       the permutation.
--     * There is never an effect parsed following the last component of the
--       permutation.
--     * No effects are intercalated between missing components with a
--       default value.

intercalateEffect
  :: ( Alternative m
     , Monad m)
  => m b             -- ^ Effect to be intercalated between permutation components
  -> Permutation m a -- ^ Permutation specification
  -> m a             -- ^ Resulting base monad capable of handling the permutation
intercalateEffect = run noEffect
   where
     noEffect = pure ()

     run :: (Alternative m, Monad m) => m c -> m b -> Permutation m a -> m a
     run headSep tailSep (P value parser) = optional headSep >>= f
       where
         f  Nothing = maybe empty pure value
         f (Just _) = optional parser >>= g
         g  Nothing = maybe empty pure value
         g (Just p) = run tailSep tailSep p

-- | \"Lifts\" a parser to a permutation parser.

toPermutation
  :: Alternative m
  => m a -- ^ Permutation component
  -> Permutation m a
toPermutation p = P Nothing $ pure <$> p

-- | \"Lifts\" a parser with a default value to a permutation parser.
--
-- If no permutation containing the supplied parser can be parsed from the input,
-- then the supplied default value is returned in lieu of a parse result.

toPermutationWithDefault
  :: Alternative m
  => a   -- ^ Default Value
  -> m a -- ^ Permutation component
  -> Permutation m a
toPermutationWithDefault v p = P (Just v) $ pure <$> p
