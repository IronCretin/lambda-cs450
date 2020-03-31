{-# LANGUAGE LambdaCase #-}

import System.Console.Haskeline
import System.IO
import Text.Read
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad

data Val
    = Num Integer
    | Clo (Map String Val) String Exp
    deriving Eq
data Exp
    = Val Val
    | Var String
    | App Exp Exp
    | Lam String Exp
    deriving Eq

instance Show Val where
    showsPrec p (Num n)     = showsPrec p n
    showsPrec p (Clo m x e) = showString "#<[" .
                              foldr (\p -> (showEntry p .)) id (M.assocs m) .
                              showString "], " .
                              showString "\\" .
                              showString x .
                              showString ". " .
                              showsPrec 0 e .
                              showString ">"
        where
            showEntry (k, a) = showString k .
                               showString " -> " .
                               showsPrec 1 a .
                               showString ";"

instance Show Exp where
    showsPrec p (Val v) = showsPrec p v
    showsPrec p (Var x) = showString x
    showsPrec p (App a b) = showParen (p >= 0) $
                                showsPrec (-1) a .
                                showString " " .
                                showsPrec 2 b
    showsPrec p (Lam x e) = showParen (p < 0 || 1 < p) $
                                showString "\\" .
                                showString x .
                                showString ". " .
                                showsPrec 0 e

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
            reserved c = isSpace c || c `elem` "()\\.#"
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
            _      -> fail "Not a value"


instance Read Exp where
    readPrec = 
        foldl1 App <$> some (foldl1 (<++)
            [ readPrec >>= \case
                TOpen -> readPrec <* require TClose
                TLam   -> readLam []
                _ -> empty
            , Val <$> readPrec
            , do
                TName n <- readPrec
                pure $ Var n
            ])
        where
            readLam args = readPrec >>= \case
                TName n -> readLam (n:args)
                TDot    -> do
                    body <- readPrec
                    pure $ foldl (flip Lam) body args
                _       -> fail "Invalid lambda"

closed :: Exp -> Bool
closed e = go e (S.empty)  where
    go (Val (Clo m x e)) bound = go e (S.insert x (S.union (M.keysSet m) bound))
    go (Lam x e)         bound = go e (S.insert x bound)
    go (Var x)           bound = S.member x bound
    go (App a b)         bound = go a bound && go b bound
    go e                 bound = True

eval :: Exp -> Maybe Val
eval e = go e M.empty  where
    go (Val v)   env = pure v
    go (Var x)   env = M.lookup x env
    go (Lam x e) env = pure $ Clo env x e
    go (App a b) env = do
        Clo m x e <- go a env
        v <- go b env
        go e (M.insert x v m)

main :: IO ()
main = runInputT (Settings noCompletion (Just ".history") True) loop  where
    loop = do
        inp <- getInputLine "> "
        case inp of
            Nothing -> pure ()
            Just i -> do
                case eval =<< readMaybe i of
                    Just v  -> outputStrLn $ show v
                    Nothing -> outputStrLn "Error"
                loop
