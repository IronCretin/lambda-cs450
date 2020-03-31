{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}

import System.Console.Haskeline
import System.IO
import Text.Read hiding (get, lift)
import qualified Text.Read as R
import Data.Char
import Data.List
import Data.Functor
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative
import Control.Monad.State

import Failure
import Parsers

data Val
    = Num Integer
    | Clo (Map String Val) String Term
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
        , R.get >>= \case
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
            reserved c = isSpace c || c `elem` "()\\.#;"
            readVar = munch1 (not . reserved)
    readListPrec = many $ readPrec


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
                b <- readPrec
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


eval :: Term -> Failure [] Val
eval e = evalStateT (go e) M.empty  where
    goE :: Exp -> StateT (Map String Val) (Failure []) Val
    goE (Val v)         = pure v
    goE (Var x)         = get >>= lift . maybeToFailure . M.lookup x
    goE (Lam x e) = do
        env <- get
        pure $ Clo env x e
    goE (App a b)       = do
        Clo m x e <- goE a
        v <- goE b
        lift $ evalStateT (go e) (M.insert x v m)
    go :: Term -> StateT (Map String Val) (Failure []) Val
    go (Exp e)   = goE e
    go (Def x e) = do
        v <- goE e
        modify (M.insert x v)
        pure Void
    go (Seq a b) = do
        go a
        go b

main :: IO ()
main = runInputT (Settings noCompletion (Just ".history") True) loop  where
    loop = do
        inp <- getInputLine "\ESC[48;5;250m\ESC[38;5;27m λ \ESC[48;5;;38;5;250m\57520 \ESC[m"
        case inp of
            Nothing -> pure ()
            Just i -> do
                case eval =<< maybeToFailure (readMaybe i) of
                    Ok v     -> outputStrLn $ show v
                    Error [] -> outputStrLn $ "Parse Error"
                    Error es -> traverse_ outputStrLn es
                loop
