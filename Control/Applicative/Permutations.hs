-- |
-- Module      :  Control.Alternative.Permutations
--
-- This module is a generalization of the package @parsec-permutation@
-- authored by Samuel Hoffstaetter:
--
-- http://hackage.haskell.org/package/parsec-permutation
--
-- This module also takes inspiration from the algorithm is described
-- in: /Parsing Permutation Phrases/, by Arthur Baars, Andres Loh and
-- Doaitse Swierstra. Published as a functional pearl at the Haskell
-- Workshop 2001.
--
-- From these two works we derive a flexible and general method for
-- parsing permutations over an 'Applicative' strucuture. Quite useful
-- in conjunction with \"Free\" constructions of Applicatives, Monads,
-- etc.
--
-- Other permutation parsing libraries tend towards using special \"almost
-- applicative\" combinators for constuction which denies the library user
-- the ability to lift and unlift permutation parsing into any Applicative
-- computational context.

module Control.Alternative.Permutations
  ( Permutation()
  , intercalateEffect
  , runPermutation
  , toPermutation
  , toPermutationWithDefault
  ) where


import Control.Applicative (Alternative(..), optional)


data Permutation m a = P (Maybe a) (m (Permutation m a))


instance Functor m => Functor (Permutation m) where

    fmap f (P v p) = P (f <$> v) (fmap f <$> p)


instance Alternative m => Applicative (Permutation m) where

    pure value = P (Just value) empty

    lhs@(P f v) <*> rhs@(P g w) = P (f <*> g) (lhsAlt <|> rhsAlt)
      where
        lhsAlt = (<*> rhs) <$> v
        rhsAlt = (lhs <*>) <$> w


-- |
-- \"Unlifts\" a permutation parser into a parser to be evaluated.
runPermutation :: (Alternative m, Monad m) => Permutation m a -> m a
runPermutation (P value parser) = optional parser >>= f
   where
      f  Nothing = maybe empty pure value
      f (Just p) = runPermParser p


-- |
-- \"Unlifts\" a permutation parser into a parser to be evaluated with an
-- intercalted effect. Useful for seperators between pemutation elements.
intercalateEffect :: (Alternative m, Monad m) => m b -> Permutation m a -> m a
intercalateEffect sep perm = run (pure ()) sep perm
   where
     run :: (Alternative m, Monad m) => m c -> m b -> Permutation m a -> m a
     run headSep tailSep (P value parser) = optional (headSep *> parser) >>= f
       where
         f  Nothing = maybe empty pure value
         f (Just p) = run tailSep tailSep p


-- |
-- \"Lifts\" a parser to a permutation parser.
toPermutation :: Alternative m => m a -> Permutationm a 
toPermutation p = P Nothing $ pure <$> p


-- |
-- \"Lifts\" a parser with a default value to a permutation parser.
--
-- If no permutation containg the supplied parser can be parsed from the input,
-- then the supplied defualt value is returned in lieu of a parse result.
toPermutationWithDefault :: Alternative m => a -> m a -> Permutation m a 
toPermutationWithDefault v p = P (Just v) $ pure <$> p
