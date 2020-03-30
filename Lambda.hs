{-# LANGUAGE LambdaCase #-}

import Text.Read
import Data.Char
import Data.List
import Control.Applicative
import Control.Monad

data Val
    = Num Integer
    | Lam String Exp
    deriving Eq
data Exp
    = Val Val
    | Var String
    | App Exp Exp
    deriving Eq

instance Show Val where
    showsPrec p (Num n)   = showsPrec p n
    showsPrec p (Lam x e) = showParen (p < 0 || 1 < p) $
                                showString "\\" .
                                showString x .
                                showString ". " .
                                showsPrec 0 e
instance Show Exp where
    showsPrec p (Val v) = showsPrec p v
    showsPrec p (Var x) = showString x
    showsPrec p (App a b) = showParen (p >= 0) $
                                showsPrec (-1) a .
                                showString " " .
                                showsPrec 2 b

data Token = TOpen | TClose | TLam | TDot | TNum Integer | TName String deriving (Eq, Show)

instance Read Token where
    readPrec = foldl1 (<++)
        [ TNum <$> readPrec
        , get >>= \case
            '('           -> pure TOpen
            ')'           -> pure TClose
            '\\'          -> pure TLam
            '.'           -> pure TDot
            w | isSpace w -> readPrec
            _             -> empty
        , TName <$> readVar
        ] where
            reserved c = isSpace c || c `elem` "()\\."
            readVar = munch1 (not . reserved)

    readListPrec = many $ readPrec

munch :: (Char -> Bool) -> ReadPrec String
munch p = look >>= \case
    c:cs | p c -> get >> (c:) <$> (munch p)
    _          -> pure []
munch1 :: (Char -> Bool) -> ReadPrec String
munch1 p = mfilter (not . null) (munch p)

require :: (Eq a, Read a) => a -> ReadPrec a
require a = mfilter (a ==) readPrec

instance Read Val where
    readPrec = do
        readPrec >>= \case
            TNum n -> pure $ Num n
            TLam   -> do
                TName arg <- readPrec
                require TDot
                body <- readPrec
                pure $ Lam arg body
            _ -> empty

instance Read Exp where
    readPrec = 
        foldl1 App <$> some (foldl1 (<++)
            [ readPrec >>= \case
                TOpen -> readPrec <* require TClose
                _ -> empty
            , Val <$> readPrec
            , do
                TName n <- readPrec
                pure $ Var n
            ])