-- |
-- Module      :  Control.Applicative.Permutations
-- Copyright   :  © 2017 Mark Karpov
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
-- /Parsing Permutation Phrases/, by Arthur Baars, Andres Loh and Doaitse
-- Swierstra. Published as a functional pearl at the Haskell Workshop 2001.
--
-- From these two works we derive a flexible and general method for parsing
-- permutations over an 'Applicative' structure. Quite useful in conjunction
-- with \"Free\" constructions of Applicatives, Monads, etc.
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
-- Equivalently, this can also be describe using the convenience operators
-- reminiscent of other parsing libraries:
--
-- > test = runPermutation $
-- >          (,,) <$?> ("", some (char 'a'))
-- >               <||> char 'b'
-- >               <|?> ('_', char 'c')
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
  , toPermutationWithDefault
    -- ** Convenience operators
  , (<$$>)
  , (<$?>)
  , (<||>)
  , (<|?>) )
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
-- For example, suppose that similar to above we want to parse a permutation of:
-- an optional string of @a@'s, the character @b@ and an optional @c@. /However/,
-- we also want each element of the permutation to be separated by a colon.
-- Using a standard parsing library combinator @char@, this can be described
-- using the 'Applicative' instance by:
--
-- > test = intercalateEffect (char ':') $
-- >          (,,) <$?> ("", some (char 'a'))
-- >               <||> char 'b'
-- >               <|?> ('_', char 'c')
--
-- This will accept strings such as: \"a:b:c\", \"b:c:a\", \"b:aa\", \"b\", etc.
--
-- Note that the effect is intercalated /between/ permutation components and that:
--
--  - There is never an effect parsed preceeding the first component of the permutation
--
--  - There is never an effect parsed following the last component of the permutation
--
--  - No effects are intercalated between missing components with a default value.

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

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation parser
-- is the function @f@ applied to the return value of @p@. The parser @p@ is
-- not allowed to accept empty input—use the optional combinator ('<$?>')
-- instead.
--
-- If the function @f@ takes more than one parameter, the type variable @b@
-- is instantiated to a functional type which combines nicely with the adds
-- parser @p@ to the ('<||>') combinator. This results in stylized code
-- where a permutation parser starts with a combining function @f@ followed
-- by the parsers. The function @f@ gets its parameters in the order in
-- which the parsers are specified, but actual input can be in any order.

(<$$>)
  :: Alternative m
  => (a -> b)        -- ^ Function to use on result of parsing
  -> m a             -- ^ Normal parser
  -> Permutation m b -- ^ Permutation parser build from it
f <$$> c = toPermutation $ f <$> c

-- | The expression @f \<$?> (x, p)@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation parser is
-- the function @f@ applied to the return value of @p@. The parser @p@ is
-- optional—if it cannot be applied, the default value @x@ will be used
-- instead.

(<$?>)
  :: Alternative m
  => (a -> b)        -- ^ Function to use on result of parsing
  -> (a, m a)        -- ^ Default value and parser
  -> Permutation m b -- ^ Permutation parser
f <$?> (v,c) = f <$> toPermutationWithDefault v c

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation parser
-- @perm@. The parser @p@ is not allowed to accept empty input—use the
-- optional combinator ('<|?>') instead. Returns a new permutation parser
-- that includes @p@.

(<||>)
  :: Alternative m
  => Permutation m (a -> b) -- ^ Given permutation parser
  -> m a                    -- ^ Parser to add (should not accept empty input)
  -> Permutation m b        -- ^ Resulting parser
p <||> c = p <*> toPermutation c

-- | The expression @perm \<||> (x, p)@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is optional—if it cannot be applied, the
-- default value @x@ will be used instead. Returns a new permutation parser
-- that includes the optional parser @p@.

(<|?>)
  :: Alternative m
  => Permutation m (a -> b) -- ^ Given permutation parser
  -> (a, m a)               -- ^ Default value and parser
  -> Permutation m b        -- ^ Resulting parser
p <|?> (v,c) = p <*> toPermutationWithDefault v c
