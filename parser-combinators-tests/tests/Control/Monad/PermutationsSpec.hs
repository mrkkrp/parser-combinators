{-# LANGUAGE TypeFamilies #-}

module Control.Monad.PermutationsSpec (spec) where

import Control.Monad
import Control.Monad.Permutations
import Data.List
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do
  describe "runPermutation & Permutation" $ do
    describe "Functor instance" $ do
      it "obeys identity law" $
        property $ \n ->
          prsp (fmap id (pure (n :: Int))) ""
            === prsp (id (pure n)) ""
      it "obeys composition law" $
        property $ \n m t ->
          let f = (+ m)
              g = (* t)
           in prs (fmap (f . g) (pure (n :: Int))) ""
                === prs ((fmap f . fmap g) (pure n)) ""
    describe "Applicative instance" $ do
      it "obeys identity law" $
        property $ \n ->
          prsp (pure id <*> pure (n :: Int)) ""
            === prsp (pure n) ""
      it "obeys composition law" $
        property $ \n m t ->
          let u = pure (+ m)
              v = pure (* t)
              w = pure (n :: Int)
           in prsp (pure (.) <*> u <*> v <*> w) ""
                === prsp (u <*> (v <*> w)) ""
      it "obeys homomorphism law" $
        property $ \x m ->
          let f = (+ m)
           in prsp (pure f <*> pure (x :: Int)) ""
                === prsp (pure (f x)) ""
      it "obeys interchange law" $
        property $ \n y ->
          let u = pure (+ n)
           in prsp (u <*> pure (y :: Int)) ""
                === prsp (pure ($ y) <*> u) ""
  describe "toPermutation" $
    it "works" $
      property $ \xs s' -> forAll (shuffle xs) $ \ys -> do
        let s = ys ++ s'
            p = foldr f (pure []) xs
            f x p' = (:) <$> toPermutation (char x) <*> p'
        prsp p s `shouldParse` xs
        prsp' p s `succeedsLeaving` s'
  describe "intercalateEffect" $
    it "works" $
      property $ \e xs s' ->
        let preconditions f =
              foldr1
                (.||.)
                [ -- Zero permutation targets will cause problems
                  property $ null xs,
                  -- If the effect is the prefix of the suffix, it will fail
                  not (null s') .&&. e == head s',
                  f
                ]
         in preconditions $
              forAll (intersperse e <$> shuffle xs) $ \ys -> do
                let s = ys ++ s'
                    p = intercalateEffect (char e) $ foldr f (pure []) xs
                    f x p' = (:) <$> toPermutation (char x) <*> p'
                prs p s `shouldParse` xs
                prs' p s `succeedsLeaving` s'
  describe "toPermutationWithDefault" $ do
    let testCases =
          [ ("abc", "abc", ""),
            ("acb", "abc", ""),
            ("bac", "abc", ""),
            ("bca", "abc", ""),
            ("cab", "abc", ""),
            ("cba", "abc", ""),
            ("aab", "ayz", "ab"),
            ("aba", "abz", "a"),
            ("baa", "abz", "a"),
            ("bba", "xbz", "ba"),
            ("bab", "abz", "b"),
            ("abb", "abz", "b"),
            ("cca", "xyc", "ca"),
            ("cac", "ayc", "c"),
            ("acc", "ayc", "c"),
            ("aaa", "ayz", "aa"),
            ("bbb", "xbz", "bb"),
            ("ccc", "xyc", "cc"),
            ("q", "xyz", "q"),
            ("", "xyz", "")
          ]
    forM_ testCases $ \(i, o, r) ->
      it ("parses \"" ++ i ++ "\" as \"" ++ o ++ "\" leaving \"" ++ r ++ "\"") $ do
        prsp testPermParser i `shouldParse` o
        prsp' testPermParser i `succeedsLeaving` r
  describe "intercalateEffect (with default)" $ do
    let p = intercalateEffect (char ',') testPermParser
        testCases =
          [ ("a,b,c", "abc", ""),
            ("a,c,b", "abc", ""),
            ("b,a,c", "abc", ""),
            ("b,c,a", "abc", ""),
            ("c,a,b", "abc", ""),
            ("c,b,a", "abc", ""),
            ("aab", "ayz", "ab"),
            ("a,ba", "abz", "a"),
            ("b,aa", "abz", "a"),
            ("bba", "xbz", "ba"),
            ("b,ab", "abz", "b"),
            ("a,bb", "abz", "b"),
            ("cca", "xyc", "ca"),
            ("c,ac", "ayc", "c"),
            ("a,cc", "ayc", "c"),
            ("aaa", "ayz", "aa"),
            ("bbb", "xbz", "bb"),
            ("ccc", "xyc", "cc"),
            ("!", "xyz", "!"),
            (",", "xyz", ","),
            ("", "xyz", "")
          ]
    forM_ testCases $ \(i, o, r) ->
      it ("parses \"" ++ i ++ "\" as \"" ++ o ++ "\" leaving \"" ++ r ++ "\"") $ do
        prs p i `shouldParse` o
        prs' p i `succeedsLeaving` r

prsp ::
  Permutation Parser a ->
  String ->
  Either (ParseErrorBundle String Void) a
prsp p = prs (runPermutation p)

prsp' ::
  Permutation Parser a ->
  String ->
  (State String Void, Either (ParseErrorBundle String Void) a)
prsp' p = prs' (runPermutation p)

testPermParser :: Permutation Parser String
testPermParser =
  f
    <$> toPermutationWithDefault 'x' (char 'a')
    <*> toPermutationWithDefault 'y' (char 'b')
    <*> toPermutationWithDefault 'z' (char 'c')
  where
    f a b c = [a, b, c]
