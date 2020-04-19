{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}

import System.Console.Haskeline
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
import Control.Applicative hiding (Const)
import Control.Monad.State

import Failure
import Parsers
import Heap
import Frame

data Val
    = Num Integer
    | Bool Bool
    | Clo (Handle (Frame Val)) String Term
    | Void
    | Fun1 String (Val -> Val)
    | Fun2 String (Val -> Val -> Val)
    | And
    | Or
    | Const Val
    -- deriving Eq
data Exp
    = Val Val
    | Var String
    | App Exp Exp
    | Lam String Term
    -- deriving Eq
data Term
    = Exp Exp
    | Seq Term Term
    | Def String Exp
    -- deriving Eq

instance Show Val where
    showsPrec p (Num n)      = showsPrec p n
    showsPrec p (Bool True)  = showString "#t"
    showsPrec p (Bool False) = showString "#f"
    showsPrec p (Clo m x e)  = showString "#<[@" .
                               showsPrec 0 (handleVal m) .
                               showString "], " .
                               showString "\\" .
                               showString x .
                               showString ". " .
                               showsPrec 0 e .
                               showString ">"
        where
            showEntry (k, a)  = showString k .
                                showString " -> " .
                                showsPrec 1 a .
                                showString ";"
    showsPrec p Void          = showString "#<void>"
    showsPrec p (Fun1 n _)    = showString "#<procedure:" .
                                showString n .
                                showString ">"
    showsPrec p (Fun2 n _)    = showString "#<procedure:" .
                                showString n .
                                showString ">"
    showsPrec p (And)         = showString "#<and>"
    showsPrec p (Or)          = showString "#<or>"
    showsPrec p (Const v)     = showString "#<const:" .
                                showsPrec 0 v .
                                showString ">"

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

data Token = TOpen | TClose | TLam | TSC | TLet | TEQ | TDot | THash String | TNum Integer | TName String  deriving (Eq, Show)

instance Read Token where
    readPrec = foldl1 (<++)
        [ TNum <$> readPrec
        , R.get >>= \case
            '('           -> pure TOpen
            ')'           -> pure TClose
            '\\'          -> pure TLam
            '.'           -> pure TDot
            ';'           -> pure TSC
            '#'           -> THash <$> readHash 0
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
            readHash 0 = look >>= \case 
                    '<':_            -> do R.get; readHash 1
                    '>':_            -> fail "unmatched brackets"
                    c:_ | reserved c -> pure []
                        | otherwise  -> do R.get; (c:) <$> readHash 0
                    []               -> pure []
            readHash l = R.get >>= \case 
                    '<'           -> ('<':) <$> readHash (l+1)
                    '>'           -> if l == 1
                        then pure []
                        else ('>':) <$> readHash (l-1)
                    c             -> (c:) <$> readHash l
    readListPrec = many $ readPrec


instance Read Val where
    readPrec = do
        readPrec >>= \case
            TNum n         -> pure $ Num n
            THash "t"      -> pure $ Bool True
            THash "f"      -> pure $ Bool False
            THash "void" -> pure Void
            _              -> fail "Not a value"

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

liftNum2 :: String -> (Integer -> Integer -> Integer) -> (String, Val)
liftNum2 n f = (n, Fun2 n $ \(Num a) (Num b) -> Num (f a b))
liftBool :: String -> (Bool -> Bool) -> (String, Val)
liftBool n f = (n, Fun1 n $ \(Bool b) -> Bool (f b))
funId, funConst :: Val
funId = Fun1 "id" id
funConst = Fun1 "const" Const
stdlib :: [(String, Val)]
stdlib =
    [ liftNum2 "+" (+)
    , liftNum2 "*" (*)
    , liftNum2 "-" (-)
    , ("and", And)
    , ("or", Or)
    , liftBool "not" not
    , ("id", funId)
    , ("const", funConst)
    , ("voif", Void)
    ]

truthy :: Val -> Bool
truthy (Bool b) = b
truthy _        = True

eval :: Term -> Failure [] Val
eval e = flip evalStateT M.empty $ do
        root <- heapAlloc frameRoot
        traverse (\(k, v) -> framePut k v root) stdlib
        evalSt root e
    where
        evalEx :: Handle (Frame Val) -> Exp -> StateT (Heap (Frame Val)) (Failure []) Val
        evalEx env (Val v)     = pure v
        evalEx env (Var x)     = frameGet x env
        evalEx env (Lam x e)   = do
            pure $ Clo env x e
        evalEx env (App a b)   = do
            a' <- evalEx env a
            case a' of
                Clo envf x body -> do
                    v <- evalEx env b
                    envb <- framePush x v envf
                    evalSt envb body
                Fun1 _ f -> f <$> evalEx env b
                Fun2 n f -> do
                    v <- evalEx env b
                    pure $ Fun1 (n ++ " " ++ show v) $ f v
                And -> do
                    v <- evalEx env b
                    if truthy v
                    then pure funId
                    else pure $ Const v
                Or -> do
                    v <- evalEx env b
                    if truthy v
                    then pure $ Const v
                    else pure funId
                Const c -> pure c
                _ -> fail $ "Not an expression: " ++ show a'

        evalSt :: Handle (Frame Val) -> Term -> StateT (Heap (Frame Val)) (Failure []) Val
        evalSt env (Exp e)   = evalEx env e
        evalSt env (Def x e) = do
            v <- evalEx env e
            framePut x v env
            pure Void
        evalSt env (Seq a b) = do
            evalSt env a
            evalSt env b

main :: IO ()
main = runInputT (Settings noCompletion (Just ".history") True) loop  where
    loop = do
        inp <- getInputLine "λ> "
        case inp of
            Nothing -> pure ()
            Just i -> do
                case eval =<< maybeToFailure (readMaybe i) of
                    Ok v     -> outputStrLn $ show v
                    Error [] -> outputStrLn $ "Parse Error"
                    Error es -> traverse_ outputStrLn es
                loop
