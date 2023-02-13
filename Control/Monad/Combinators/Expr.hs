-- |
-- Module      :  Control.Monad.Combinators.Expr
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A helper module to parse expressions. It can build a parser given a table
-- of operators.
--
-- @since 1.0.0
module Control.Monad.Combinators.Expr
  ( Operator (..),
    makeExprParser,
  )
where

import Control.Monad
import Control.Monad.Combinators

-- | This data type specifies operators that work on values of type @a@. An
-- operator is either binary infix or unary prefix or postfix. A binary
-- operator has also an associated associativity.
data Operator m a
  = -- | Non-associative infix
    InfixN (m (a -> a -> a))
  | -- | Left-associative infix
    InfixL (m (a -> a -> a))
  | -- | Right-associative infix
    InfixR (m (a -> a -> a))
  | -- | Prefix
    Prefix (m (a -> a))
  | -- | Postfix
    Postfix (m (a -> a))
  | -- | Right-associative ternary. Right-associative means that
    -- @a ? b : d ? e : f@ parsed as
    -- @a ? b : (d ? e : f)@ and not as @(a ? b : d) ? e : f@.
    --
    -- The outer monadic action parses the first separator (e.g. @?@) and
    -- returns an action (of type @m (a -> a -> a -> a)@) that parses the
    -- second separator (e.g. @:@).
    --
    -- Example usage:
    --
    -- >>> TernR ((If <$ char ':') <$ char '?')
    TernR (m (m (a -> a -> a -> a)))

-- | @'makeExprParser' term table@ builds an expression parser for terms
-- @term@ with operators from @table@, taking the associativity and
-- precedence specified in the @table@ into account.
--
-- @table@ is a list of @[Operator m a]@ lists. The list is ordered in
-- descending precedence. All operators in one list have the same precedence
-- (but may have different associativity).
--
-- Prefix and postfix operators of the same precedence associate to the left
-- (i.e. if @++@ is postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- Unary operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). If you need to parse several prefix
-- or postfix operators in a row, (like C pointers—@**i@) you can use this
-- approach:
--
-- > manyUnaryOp = foldr1 (.) <$> some singleUnaryOp
--
-- This is not done by default because in some cases allowing repeating
-- prefix or postfix operators is not desirable.
--
-- If you want to have an operator that is a prefix of another operator in
-- the table, use the following (or similar) wrapper (Megaparsec example):
--
-- > op n = (lexeme . try) (string n <* notFollowedBy punctuationChar)
--
-- 'makeExprParser' takes care of all the complexity involved in building an
-- expression parser. Here is an example of an expression parser that
-- handles prefix signs, postfix increment and basic arithmetic:
--
-- > expr = makeExprParser term table <?> "expression"
-- >
-- > term = parens expr <|> integer <?> "term"
-- >
-- > table = [ [ prefix  "-"  negate
-- >           , prefix  "+"  id ]
-- >         , [ postfix "++" (+1) ]
-- >         , [ binary  "*"  (*)
-- >           , binary  "/"  div  ]
-- >         , [ binary  "+"  (+)
-- >           , binary  "-"  (-)  ] ]
-- >
-- > binary  name f = InfixL  (f <$ symbol name)
-- > prefix  name f = Prefix  (f <$ symbol name)
-- > postfix name f = Postfix (f <$ symbol name)
makeExprParser ::
  (MonadPlus m) =>
  -- | Term parser
  m a ->
  -- | Operator table, see 'Operator'
  [[Operator m a]] ->
  -- | Resulting expression parser
  m a
makeExprParser = foldl addPrecLevel
{-# INLINEABLE makeExprParser #-}

-- | @addPrecLevel p ops@ adds the ability to parse operators in table @ops@
-- to parser @p@.
addPrecLevel :: (MonadPlus m) => m a -> [Operator m a] -> m a
addPrecLevel term ops =
  term' >>= \x -> choice [ras' x, las' x, nas' x, tern' x, return x]
  where
    (ras, las, nas, prefix, postfix, tern) = foldr splitOp ([], [], [], [], [], []) ops
    term' = pTerm (choice prefix) term (choice postfix)
    ras' = pInfixR (choice ras) term'
    las' = pInfixL (choice las) term'
    nas' = pInfixN (choice nas) term'
    tern' = pTernR (choice tern) term'
{-# INLINEABLE addPrecLevel #-}

-- | @pTerm prefix term postfix@ parses a @term@ surrounded by optional
-- prefix and postfix unary operators. Parsers @prefix@ and @postfix@ are
-- allowed to fail, in this case 'id' is used.
pTerm :: (MonadPlus m) => m (a -> a) -> m a -> m (a -> a) -> m a
pTerm prefix term postfix = do
  pre <- option id prefix
  x <- term
  post <- option id postfix
  return . post . pre $ x
{-# INLINE pTerm #-}

-- | @pInfixN op p x@ parses non-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.
pInfixN :: (MonadPlus m) => m (a -> a -> a) -> m a -> a -> m a
pInfixN op p x = do
  f <- op
  y <- p
  return $ f x y
{-# INLINE pInfixN #-}

-- | @pInfixL op p x@ parses left-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.
pInfixL :: (MonadPlus m) => m (a -> a -> a) -> m a -> a -> m a
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p r <|> return r
{-# INLINE pInfixL #-}

-- | @pInfixR op p x@ parses right-associative infix operator @op@, then
-- term with parser @p@, then returns result of the operator application on
-- @x@ and the term.
pInfixR :: (MonadPlus m) => m (a -> a -> a) -> m a -> a -> m a
pInfixR op p x = do
  f <- op
  y <- p >>= \r -> pInfixR op p r <|> return r
  return $ f x y
{-# INLINE pInfixR #-}

-- | Parse the first separator of a ternary operator
pTernR :: (MonadPlus m) => m (m (a -> a -> a -> a)) -> m a -> a -> m a
pTernR sep1 p x = do
  sep2 <- sep1
  y <- p >>= \r -> pTernR sep1 p r `mplus` return r
  f <- sep2
  z <- p >>= \r -> pTernR sep1 p r `mplus` return r
  return $ f x y z
{-# INLINE pTernR #-}

type Batch m a =
  ( [m (a -> a -> a)],
    [m (a -> a -> a)],
    [m (a -> a -> a)],
    [m (a -> a)],
    [m (a -> a)],
    [m (m (a -> a -> a -> a))]
  )

-- | A helper to separate various operators (binary, unary, and according to
-- associativity) and return them in a tuple.
splitOp :: Operator m a -> Batch m a -> Batch m a
splitOp (InfixR op) (r, l, n, pre, post, tern) = (op : r, l, n, pre, post, tern)
splitOp (InfixL op) (r, l, n, pre, post, tern) = (r, op : l, n, pre, post, tern)
splitOp (InfixN op) (r, l, n, pre, post, tern) = (r, l, op : n, pre, post, tern)
splitOp (Prefix op) (r, l, n, pre, post, tern) = (r, l, n, op : pre, post, tern)
splitOp (Postfix op) (r, l, n, pre, post, tern) = (r, l, n, pre, op : post, tern)
splitOp (TernR op) (r, l, n, pre, post, tern) = (r, l, n, pre, post, op : tern)
