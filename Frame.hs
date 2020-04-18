{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module Frame where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Applicative

import Heap
    
data Frame a = Frame
    { parent :: Maybe (Handle (Frame a))
    , locals :: Map String a
    } deriving (Show, Eq, Ord)

frameRoot :: Frame a
frameRoot = Frame Nothing M.empty

framePush :: (MonadState (Heap (Frame a)) m, MonadPlus m) => String -> a -> Handle (Frame a) -> m (Handle (Frame a))
framePush k v p = do
    heapAlloc (Frame (Just p) (M.singleton k v))

framePut :: (MonadState (Heap (Frame a)) m, MonadPlus m) => String -> a -> Handle (Frame a) -> m ()
framePut k v h = do
    f <- heapGet h
    heapPut h (f { locals = M.insert k v (locals f) })

frameGet :: (MonadState (Heap (Frame a)) m, MonadPlus m) => String -> Handle (Frame a) -> m a
frameGet k h = do
    f <- heapGet h
    case M.lookup k (locals f) of
        Just v  -> pure v
        Nothing -> case parent f of
            Just p -> frameGet k p
            Nothing -> fail $ "Couldn't find " ++ k