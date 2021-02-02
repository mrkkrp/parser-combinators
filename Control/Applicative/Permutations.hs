{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      :  Control.Applicative.Permutations
-- Copyright   :  © 2017–present Alex Washburn
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
-- parsing library combinator @char@ (e.g. 'Text.ParserCombinators.ReadP.ReadP')
-- this can be described using the 'Applicative' instance by:
--
-- > test = runPermutation $
-- >          (,,) <$> toPermutationWithDefault ""  (some (char 'a'))
-- >               <*> toPermutation (char 'b')
-- >               <*> toPermutationWithDefault '_' (char 'c')
--
-- @since 0.2.0
module Control.Applicative.Permutations
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
import Data.Function ((&))

-- | An 'Applicative' wrapper-type for constructing permutation parsers.
data Permutation m a = P !(Maybe a) [Branch m a]

data Branch m a = forall z. Branch (Permutation m (z -> a)) (m z)

instance Functor m => Functor (Permutation m) where
  fmap f (P v bs) = P (f <$> v) (fmap f <$> bs)

instance Functor p => Functor (Branch p) where
  fmap f (Branch perm p) = Branch (fmap (f .) perm) p

instance Functor m => Applicative (Permutation m) where
  pure value = P (Just value) empty
  lhs@(P f v) <*> rhs@(P g w) = P (f <*> g) $ (ins2 <$> v) <> (ins1 <$> w)
    where
      ins1 (Branch perm p) = Branch ((.) <$> lhs <*> perm) p
      ins2 (Branch perm p) = Branch (flip <$> perm <*> rhs) p
  liftA2 f lhs@(P x v) rhs@(P y w) = P (liftA2 f x y) $ (ins2 <$> v) <> (ins1 <$> w)
    where
      ins1 (Branch perm p) = Branch (liftA2 ((.) . f) lhs perm) p
      ins2 (Branch perm p) = Branch (liftA2 (\b g z -> f (g z) b) rhs perm) p

-- | \"Unlifts\" a permutation parser into a parser to be evaluated.
runPermutation ::
  Alternative m =>
  -- | Permutation specification
  Permutation m a ->
  -- | Resulting base monad capable of handling the permutation
  m a
runPermutation = foldAlt f
  where
    -- INCORRECT   = runPerms t <*> p
    f (Branch t p) = (&) <$> p <*> runPermutation t

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
--     * If an effect is encountered after a component, another component must
--       immediately follow the effect.
intercalateEffect ::
  Alternative m =>
  -- | Effect to be intercalated between permutation components
  m b ->
  -- | Permutation specification
  Permutation m a ->
  -- | Resulting base applicative capable of handling the permutation
  m a
intercalateEffect effect = foldAlt (runBranchEff effect)
  where
    runPermEff :: Alternative m => m b -> Permutation m a -> m a
    runPermEff eff (P v bs) =
      eff *> foldr ((<|>) . runBranchEff eff) empty bs <|> maybe empty pure v

    runBranchEff :: Alternative m => m b -> Branch m a -> m a
    runBranchEff eff (Branch t p) = (&) <$> p <*> runPermEff eff t

-- | \"Lifts\" a parser to a permutation parser.
toPermutation ::
  Alternative m =>
  -- | Permutation component
  m a ->
  Permutation m a
toPermutation = P Nothing . pure . branch

-- | \"Lifts\" a parser with a default value to a permutation parser.
--
-- If no permutation containing the supplied parser can be parsed from the input,
-- then the supplied default value is returned in lieu of a parse result.
toPermutationWithDefault ::
  Alternative m =>
  -- | Default Value
  a ->
  -- | Permutation component
  m a ->
  Permutation m a
toPermutationWithDefault v = P (Just v) . pure . branch

branch :: Functor m => m a -> Branch m a
branch = Branch $ pure id

foldAlt :: Alternative m => (Branch m a -> m a) -> Permutation m a -> m a
foldAlt f (P v bs) = foldr ((<|>) . f) (maybe empty pure v) bs
