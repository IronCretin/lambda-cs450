{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}

module Sexp (Sexp(..)) where

import Text.Read
import Control.Applicative (many, empty, (<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Parsers

data Tok = TI Integer | TF Double | TP String | TN String | TH String | TS String   deriving (Show, Eq)

instance Read Tok where
    readPrec = foldr1 (<++)
        [ TI <$> readPrec
        , TF <$> readPrec
        , TS <$> readPrec
        , TN <$> readVar
        , get >>= \case
            '('  -> pure $ TP "("
            ')'  -> pure $ TP ")"
            '['  -> pure $ TP "["
            ']'  -> pure $ TP "]"
            '{'  -> pure $ TP "{"
            '}'  -> pure $ TP "}"
            '\'' -> pure $ TP "'"
            '-'  -> pure $ TN "-"
            '#'  -> TH <$> readHash 0
            ';'  -> munch (/= '\n') >> readPrec
            c | isSpace c -> readPrec
            _    -> empty
        ]
        where
            reserved c = isSpace c || c `elem` "()[]{};|#'"
            readVar = munch1 (not . reserved)
                <|> do
                    '|' <- get
                    n <- munch1 (/= '|')
                    '|' <- get
                    pure n
            readHash 0 = look >>= \case 
                    '<':_            -> (:) <$> get <*> readHash 1
                    '>':_            -> fail "unmatched brackets"
                    c:_ | reserved c -> pure []
                        | otherwise  -> (:) <$> get <*> readHash 0
                    []               -> pure []
            readHash l = get >>= \case 
                    '<'           -> ('<':) <$> readHash (l+1)
                    '>'           -> if l == 1
                        then pure ">"
                        else ('>':) <$> readHash (l-1)
                    c             -> (c:) <$> readHash l
    readListPrec = many readPrec


data Sexp = I Integer | F Double | L [Sexp] | N String | H String | S String  deriving Show

instance Read Sexp where
    readPrec = readPrec >>= \case
        TI x -> pure $ I x
        TF x -> pure $ F x
        TN x -> pure $ N x
        TH x -> pure $ H x
        TS x -> pure $ S x
        TP "(" -> readL ")"
        TP "[" -> readL "]"
        TP "{" -> readL "}"
        _ -> empty
    readListPrec = many readPrec

readL cl = do
    exs <- many readPrec
    require $ TP cl
    pure $ L exs