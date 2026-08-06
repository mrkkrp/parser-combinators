{-# LANGUAGE MultiWayIf #-}

module Control.Applicative.CombinatorsSpec (spec) where

import Control.Applicative.Combinators
import Data.Char (isDigit, isLetter)
import Data.List (intersperse)
import Data.Maybe (fromJust, fromMaybe, isNothing, maybeToList)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec.Char

spec :: Spec
spec = do

  describe "logical" $ do
    let
      b  = undefined <$ char 'u' <|> False <$ char 'f' <|> True <$ char 't'
      ux = utok 'x'
      eb = etok 'u' <> etok 'f' <> etok 't'

    describe "<&&>" $ do
      let p = b <&&> b
      it "works" $ do
        prs_ p "t"  `shouldFailWith` err 1 (eb <> ueof)
        prs_ p "tf" `shouldParse`    False
        prs_ p "tt" `shouldParse`    True
        prs_ p "tx" `shouldFailWith` err 1 (eb <> ux)
        prs_ p "x"  `shouldFailWith` err 0 (eb <> ux)
      it "does not short-circuit" $ do
        prs_ p "f"  `shouldFailWith` err 1 (eb <> ueof)
        prs_ p "ff" `shouldParse`    False
        prs_ p "ft" `shouldParse`    False
        prs_ p "fu" `shouldParse`    False
        prs_ p "fx" `shouldFailWith` err 1 (eb <> ux)

    describe "<||>" $ do
      let p = b <||> b
      it "works" $ do
        prs_ p "f"  `shouldFailWith` err 1 (eb <> ueof)
        prs_ p "ff" `shouldParse`    False
        prs_ p "ft" `shouldParse`    True
        prs_ p "fx" `shouldFailWith` err 1 (eb <> ux)
        prs_ p "x"  `shouldFailWith` err 0 (eb <> ux)
      it "does not short-circuit" $ do
        prs_ p "t"  `shouldFailWith` err 1 (eb <> ueof)
        prs_ p "tf" `shouldParse`    True
        prs_ p "tt" `shouldParse`    True
        prs_ p "tu" `shouldParse`    True
        prs_ p "tx" `shouldFailWith` err 1 (eb <> ux)

  describe "between" $
    it "works" . property $ \pre c n' post -> do
      let p = between (string pre) (string post) (many (char c))
          n = getNonNegative n'
          b = length (takeWhile (== c) post)
          z = replicate n c
          s = pre ++ z ++ post
      if b > 0
        then
          prs_ p s
            `shouldFailWith` err
              (length pre + n + b)
              ( etoks post
                  <> etok c
                  <> if length post == b
                    then ueof
                    else utoks (drop b post)
              )
        else prs_ p s `shouldParse` z

  describe "choice" $
    it "works" . property $ \cs' s' -> do
      let cs = getNonEmpty cs'
          p = choice (char <$> cs)
          s = [s']
      if s' `elem` cs
        then prs_ p s `shouldParse` s'
        else prs_ p s `shouldFailWith` err 0 (utok s' <> mconcat (etok <$> cs))

  describe "count" $ do
    it "works" . property $ \n x' -> do
      let x = getNonNegative x'
          p = count n (char 'x')
          p' = count' n n (char 'x')
          s = replicate x 'x'
      prs_ p s `shouldBe` prs_ p' s
    rightOrder (count 3 letterChar) "abc" "abc"

  describe "count'" $ do
    it "works" . property $ \m n x' -> do
      let x = getNonNegative x'
          p = count' m n (char 'x')
          s = replicate x 'x'
      if
        | n <= 0 || m > n ->
            if x == 0
              then prs_ p s `shouldParse` ""
              else prs_ p s `shouldFailWith` err 0 (utok 'x' <> eeof)
        | m <= x && x <= n ->
            prs_ p s `shouldParse` s
        | x < m ->
            prs_ p s `shouldFailWith` err x (ueof <> etok 'x')
        | otherwise ->
            prs_ p s `shouldFailWith` err n (utok 'x' <> eeof)
    rightOrder (count' 1 3 letterChar) "abc" "abc"

  describe "eitherP" $
    it "works" . property $ \ch -> do
      let p = eitherP letterChar digitChar
          s = pure ch
      if
        | isLetter ch -> prs_ p s `shouldParse` Left ch
        | isDigit ch -> prs_ p s `shouldParse` Right ch
        | otherwise ->
            prs_ p s
              `shouldFailWith` err 0 (utok ch <> elabel "letter" <> elabel "digit")

  describe "endBy" $ do
    it "works" . property $ \n' c -> do
      let n = getNonNegative n'
          p = endBy (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ [c]
      if
        | c == 'a' && n == 0 ->
            prs_ p s `shouldFailWith` err 1 (ueof <> etok '-')
        | c == 'a' ->
            prs_ p s `shouldFailWith` err (g n) (utok 'a' <> etok '-')
        | c == '-' && n == 0 ->
            prs_ p s `shouldFailWith` err 0 (utok '-' <> etok 'a' <> eeof)
        | c /= '-' ->
            prs_ p s
              `shouldFailWith` err
                (g n)
                ( utok c
                    <> (if n > 0 then etok '-' else eeof)
                    <> (if n == 0 then etok 'a' else mempty)
                )
        | otherwise -> prs_ p s `shouldParse` replicate n 'a'
    rightOrder (endBy letterChar (char ',')) "a,b,c," "abc"

  describe "endBy1" $ do
    it "works" . property $ \n' c -> do
      let n = getNonNegative n'
          p = endBy1 (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ [c]
      if
        | c == 'a' && n == 0 ->
            prs_ p s `shouldFailWith` err 1 (ueof <> etok '-')
        | c == 'a' ->
            prs_ p s `shouldFailWith` err (g n) (utok 'a' <> etok '-')
        | c == '-' && n == 0 ->
            prs_ p s `shouldFailWith` err 0 (utok '-' <> etok 'a')
        | c /= '-' ->
            prs_ p s
              `shouldFailWith` err
                (g n)
                ( utok c
                    <> (if n > 0 then etok '-' else mempty)
                    <> (if n == 0 then etok 'a' else mempty)
                )
        | otherwise -> prs_ p s `shouldParse` replicate n 'a'
    rightOrder (endBy1 letterChar (char ',')) "a,b,c," "abc"

  describe "manyTill" $ do
    it "works" . property $ \(NonNegative a) (NonNegative b) (NonNegative c) -> do
      let p = (,) <$> manyTill letterChar (char 'c') <*> many letterChar
          s = abcRow a b c
      if c == 0
        then
          prs_ p s
            `shouldFailWith` err
              (a + b)
              (ueof <> etok 'c' <> elabel "letter")
        else
          let (pre, post) = break (== 'c') s
           in prs_ p s `shouldParse` (pre, drop 1 post)
    rightOrder (manyTill letterChar (char 'd')) "abcd" "abc"

  describe "manyTill_" $ do
    it "works" . property $ \(NonNegative a) (NonNegative b) (NonNegative c) -> do
      let p = (,) <$> manyTill_ letterChar (char 'c') <*> many letterChar
          s = abcRow a b c
      if c == 0
        then
          prs_ p s
            `shouldFailWith` err
              (a + b)
              (ueof <> etok 'c' <> elabel "letter")
        else
          let (pre, post) = break (== 'c') s
           in prs_ p s `shouldParse` ((pre, 'c'), drop 1 post)
    rightOrder (fst <$> manyTill_ letterChar (char 'd')) "abcd" "abc"

  describe "someTill" $ do
    it "works" . property $ \(NonNegative a) (NonNegative b) (NonNegative c) -> do
      let p = (,) <$> someTill letterChar (char 'c') <*> many letterChar
          s = abcRow a b c
      if
        | null s ->
            prs_ p s `shouldFailWith` err 0 (ueof <> elabel "letter")
        | c == 0 ->
            prs_ p s
              `shouldFailWith` err
                (a + b)
                (ueof <> etok 'c' <> elabel "letter")
        | s == "c" ->
            prs_ p s `shouldFailWith` err 1 (ueof <> etok 'c' <> elabel "letter")
        | head s == 'c' ->
            prs_ p s `shouldParse` ("c", drop 2 s)
        | otherwise ->
            let (pre, post) = break (== 'c') s
             in prs_ p s `shouldParse` (pre, drop 1 post)
    rightOrder (someTill letterChar (char 'd')) "abcd" "abc"

  describe "someTill_" $ do
    it "works" . property $ \(NonNegative a) (NonNegative b) (NonNegative c) -> do
      let p = (,) <$> someTill_ letterChar (char 'c') <*> many letterChar
          s = abcRow a b c
      if
        | null s ->
            prs_ p s `shouldFailWith` err 0 (ueof <> elabel "letter")
        | c == 0 ->
            prs_ p s
              `shouldFailWith` err
                (a + b)
                (ueof <> etok 'c' <> elabel "letter")
        | s == "c" ->
            prs_ p s `shouldFailWith` err 1 (ueof <> etok 'c' <> elabel "letter")
        | head s == 'c' ->
            prs_ p s `shouldParse` (("c", 'c'), drop 2 s)
        | otherwise ->
            let (pre, post) = break (== 'c') s
             in prs_ p s `shouldParse` ((pre, 'c'), drop 1 post)
    rightOrder (fst <$> someTill_ letterChar (char 'd')) "abcd" "abc"

  describe "option" $
    it "works" . property $ \d a s -> do
      let p = option d (string a)
          p' = fromMaybe d <$> optional (string a)
      prs_ p s `shouldBe` prs_ p' s

  describe "sepBy" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepBy (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if
        | isNothing c' ->
            prs_ p s `shouldParse` replicate n 'a'
        | c == 'a' && n == 0 ->
            prs_ p s `shouldParse` "a"
        | n == 0 ->
            prs_ p s `shouldFailWith` err 0 (utok c <> etok 'a' <> eeof)
        | c == '-' ->
            prs_ p s `shouldFailWith` err (length s) (ueof <> etok 'a')
        | otherwise ->
            prs_ p s `shouldFailWith` err (g n) (utok c <> etok '-' <> eeof)
    rightOrder (sepBy letterChar (char ',')) "a,b,c" "abc"

  describe "sepBy1" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepBy1 (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if
        | isNothing c' && n >= 1 ->
            prs_ p s `shouldParse` replicate n 'a'
        | isNothing c' ->
            prs_ p s `shouldFailWith` err 0 (ueof <> etok 'a')
        | c == 'a' && n == 0 ->
            prs_ p s `shouldParse` "a"
        | n == 0 ->
            prs_ p s `shouldFailWith` err 0 (utok c <> etok 'a')
        | c == '-' ->
            prs_ p s `shouldFailWith` err (length s) (ueof <> etok 'a')
        | otherwise ->
            prs_ p s `shouldFailWith` err (g n) (utok c <> etok '-' <> eeof)
    rightOrder (sepBy1 letterChar (char ',')) "a,b,c" "abc"

  describe "sepEndBy" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepEndBy (char 'a') (char '-')
          a = replicate n 'a'
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if
        | isNothing c' ->
            prs_ p s `shouldParse` a
        | c == 'a' && n == 0 ->
            prs_ p s `shouldParse` "a"
        | n == 0 ->
            prs_ p s `shouldFailWith` err 0 (utok c <> etok 'a' <> eeof)
        | c == '-' ->
            prs_ p s `shouldParse` a
        | otherwise ->
            prs_ p s `shouldFailWith` err (g n) (utok c <> etok '-' <> eeof)
    rightOrder (sepEndBy letterChar (char ',')) "a,b,c," "abc"

  describe "sepEndBy1" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepEndBy1 (char 'a') (char '-')
          a = replicate n 'a'
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if
        | isNothing c' && n >= 1 ->
            prs_ p s `shouldParse` a
        | isNothing c' ->
            prs_ p s `shouldFailWith` err 0 (ueof <> etok 'a')
        | c == 'a' && n == 0 ->
            prs_ p s `shouldParse` "a"
        | n == 0 ->
            prs_ p s `shouldFailWith` err 0 (utok c <> etok 'a')
        | c == '-' ->
            prs_ p s `shouldParse` a
        | otherwise ->
            prs_ p s `shouldFailWith` err (g n) (utok c <> etok '-' <> eeof)
    rightOrder (sepEndBy1 letterChar (char ',')) "a,b,c," "abc"

  describe "skipMany" $
    it "works" . property $ \c n' a -> do
      let p = skipMany (char c) *> string a
          n = getNonNegative n'
          p' = many (char c) >> string a
          s = replicate n c ++ a
      prs_ p s `shouldBe` prs_ p' s

  describe "skipSome" $
    it "works" . property $ \c n' a -> do
      let p = skipSome (char c) *> string a
          n = getNonNegative n'
          p' = some (char c) >> string a
          s = replicate n c ++ a
      prs_ p s `shouldBe` prs_ p' s

  describe "skipCount" $
    it "works" . property $ \c n' a -> do
      let p = skipCount n (char c) *> string a
          n = getNonNegative n'
          p' = count n (char c) *> string a
          s = replicate n c ++ a
      prs_ p s `shouldBe` prs_ p' s

  describe "skipManyTill" $
    it "works" . property $ \c n' a ->
      c /= a ==> do
        let p = skipManyTill (char c) (char a)
            n = getNonNegative n'
            s = replicate n c ++ [a]
        prs_ p s `shouldParse` a

  describe "skipSomeTill" $
    it "works" . property $ \c n' a ->
      c /= a ==> do
        let p = skipSomeTill (char c) (char a)
            n = getNonNegative n'
            s = replicate n c ++ [a]
        if n == 0
          then prs_ p s `shouldFailWith` err 0 (utok a <> etok c)
          else prs_ p s `shouldParse` a

----------------------------------------------------------------------------
-- Helpers

g :: Int -> Int
g x = x + if x > 0 then x - 1 else 0
