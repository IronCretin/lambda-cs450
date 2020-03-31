{-# LANGUAGE LambdaCase #-}

import System.Console.Haskeline
import System.IO
import Text.Read
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
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
            TLam   -> readLam []
            _      -> fail "Not a value"
        where
            readLam args = readPrec >>= \case
                TName n -> readLam (n:args)
                TDot    -> do
                    body <- readPrec
                    Val v <- pure $ foldl ((Val .) . flip Lam) body args
                    pure v
                _       -> fail "Invalid lambda"


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

closed :: Exp -> Bool
closed e = go e (S.empty)  where
    go (Val (Lam x e)) bound = go e (S.insert x bound)
    go (Var x)         bound = S.member x bound
    go (App a b)       bound = go a bound && go b bound
    go e               bound = True

subst :: Exp -> String -> Val -> Exp
subst (Val (Num n))   x v             = Val (Num n)
subst (Val (Lam y e)) x v | x == y    = Val (Lam y e)
                          | otherwise = Val (Lam y (subst e x v))
subst (Var y) x v         | x == y    = Val v
                          | otherwise = Var y
subst (App a b)       x v             = App (subst a x v) (subst b x v)
    


eval :: Exp -> Maybe Val
eval (Val v)   = pure v
eval (App a b) = do
    Lam x e <- eval a
    v <- eval b
    eval $ subst e x v
eval _         = empty

main :: IO ()
main = runInputT (Settings noCompletion (Just ".history") True) loop  where
    loop = do
        inp <- getInputLine "> "
        case inp of
            Nothing -> pure ()
            Just i -> do
                case eval (read i) of
                    Just v  -> outputStrLn $ show v
                    Nothing -> outputStrLn "Error"
                loop
