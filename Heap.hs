{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M

type Heap a = M.Map (Handle a) a
newtype Handle a = Handle {handleVal :: Int} deriving (Eq, Ord, Show, Read)

emptyHeap :: Heap a
emptyHeap = M.empty

heapAlloc :: MonadState (Heap a) m => a -> m (Handle a)
heapAlloc a = do
    h <- gets $ Handle . M.size
    modify (M.insert h a)
    pure h

heapPut :: (MonadState (Heap a) m, MonadPlus m) => (Handle a) -> a -> m ()
heapPut h a = gets (M.member h) >>= \case
    True  -> modify (M.insert h a)
    False -> empty

heapGet :: (MonadState (Heap a) m, MonadPlus m) => (Handle a) -> m a
heapGet h = gets (M.lookup h) >>= \case
    Just a  -> pure a
    Nothing -> empty

ex :: StateT (Heap Int) Maybe Int
ex = do
    x <- heapAlloc 1
    heapPut x 3
    heapGet x