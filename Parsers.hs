{-# LANGUAGE LambdaCase #-}

module Parsers
( munch
, munch1
, require
, char
, string
) where

import Text.Read
import Control.Monad

munch :: (Char -> Bool) -> ReadPrec String
munch p = look >>= \case
    c:cs | p c -> get >> (c:) <$> (munch p)
    _          -> pure []
munch1 :: (Char -> Bool) -> ReadPrec String
munch1 p = mfilter (not . null) (munch p)

require :: (Eq a, Read a) => a -> ReadPrec a
require a = mfilter (a ==) readPrec

char :: Char -> ReadPrec Char
char c = mfilter (c ==) get
string :: String -> ReadPrec String
string s = traverse char s