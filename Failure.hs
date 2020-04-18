{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Failure ( Failure(..), maybeToFailure ) where

import qualified Control.Monad.Fail as F
import Control.Applicative
import Control.Monad

data Failure t a = Error (t String) | Ok a  deriving (Functor, Foldable)

deriving instance (Show (t String), Show a) => Show (Failure t a) -- where
    -- showsPrec p _ = showsPrec p p

instance Alternative t => Applicative (Failure t) where
    pure = Ok
    Error e1 <*> Error e2 = Error (e1 <|> e2)
    Error e1 <*> _        = Error e1
    _        <*> Error e2 = Error e2
    Ok f     <*> Ok x     = Ok (f x)

instance Alternative t => Alternative (Failure t) where
    empty = Error empty
    Ok a     <|> _        = Ok a
    _        <|> Ok b     = Ok b
    Error e1 <|> Error e2 = Error (e1 <|> e2)

instance Alternative t => MonadPlus (Failure t)

instance Alternative t => Monad (Failure t) where
    Ok a    >>= f = f a
    Error e >>= _ = Error e
    (>>) = (*>)
    fail = F.fail

instance Alternative t => F.MonadFail (Failure t) where
    fail = Error . pure

maybeToFailure :: Alternative t => Maybe a -> Failure t a
maybeToFailure (Just a) = pure a
maybeToFailure Nothing  = empty