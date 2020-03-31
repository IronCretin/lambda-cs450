{-# LANGUAGE LambdaCase #-}

import System.Console.Haskeline
import System.IO
import Text.Read
import Data.Char
import Data.List
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad

data Val
    = Num Integer
    | Clo (Map String Val) String Exp
    | Void
    deriving Eq
data Exp
    = Val Val
    | Var String
    | App Exp Exp
    | Lam String Term
    deriving Eq
data Term
    = Exp Exp
    | Seq Term Term
    | Def String Exp
    deriving Eq

instance Show Val where
    showsPrec p (Num n)     = showsPrec p n
    showsPrec p (Clo m x e) = showString "<" .
                              showsPrec 0 (M.assocs m) .
                              showString ", " .
                              showString "\\" .
                              showString x .
                              showString ". " .
                              showsPrec 0 e .
                              showString ">"
    showsPrec p Void        = showString "#<void>"

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

instance Show Term where
    showsPrec p (Exp e)   = showsPrec 1 e
    showsPrec p (Seq a b) = showParen (p > 1) $
                                showsPrec 0 a .
                                (if (p == 0)
                                    then showString ";\n"
                                    else showString ";") .
                                showsPrec 0 b
    showsPrec p (Def x e) = showString "let " .
                            showString x .
                            showString " := " .
                            showsPrec 2 e

data Token = TOpen | TClose | TLam | TSC | TLet | TEQ | TDot | TNum Integer | TName String  deriving (Eq, Show)

instance Read Token where
    readPrec = foldl1 (<++)
        [ TNum <$> readPrec
        , get >>= \case
            '('           -> pure TOpen
            ')'           -> pure TClose
            '\\'          -> pure TLam
            '.'           -> pure TDot
            ';'           -> pure TSC
            w | isSpace w -> readPrec
            _             -> empty
        , readVar <&> \case
            "let" -> TLet
            ":="  -> TEQ
            v     -> TName v
        , TLet <$ string "let"
        , TEQ <$ string ":="
        ] where
            reserved c = isSpace c || c `elem` "()\\.#q;"
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

char :: Char -> ReadPrec Char
char c = mfilter (c ==) get
string :: String -> ReadPrec String
string s = traverse char s

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
                TLam  -> readLam []
                _ -> empty
            , Val <$> readPrec
            , do
                TName n <- readPrec
                pure $ Var n
            ])
        where
            readLam :: [String] -> ReadPrec Exp
            readLam args = readPrec >>= \case
                TName n -> readLam (n:args)
                TDot    -> do
                    body <- readPrec
                    pure $ foldl (\e x -> Lam x (Exp e)) body args
                _       -> fail "Invalid lambda"

instance Read Term where
    readPrec = do
        a <- readTerm
        foldl1 (<++)
            [ do
                require TSC
                b <- readTerm
                pure $ Seq a b
            , pure a
            ]  where
            readTerm = foldl1 (<++)
                [ do
                    require TLet
                    TName x <- readPrec
                    require TEQ
                    e <- readPrec
                    pure $ Def x e
                , Exp <$> readPrec
                ]

-- closed :: Exp -> Bool
-- closed e = go e (S.empty)  where
--     go (Val (Clo m x e)) bound = go e (S.insert x (S.union (M.keysSet m) bound))
--     go (Lam x e)         bound = go e (S.insert x bound)
--     go (Var x)           bound = S.member x bound
--     go (App a b)         bound = go a bound && go b bound
--     go e                 bound = True

eval :: Exp -> Maybe Val
eval e = go e M.empty  where
    go (Val v)         env = pure v
    go (Var x)         env = M.lookup x env
    go (Lam x (Exp e)) env = pure $ Clo env x e
    go (App a b)       env = do
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
                case eval (read i) of
                    Just v  -> outputStrLn $ show v
                    Nothing -> outputStrLn "Error"
                loop
