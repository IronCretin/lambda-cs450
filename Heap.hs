{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M

type Heap a = M.Map Handle a
type Handle = Int

emptyHeap :: Heap a
emptyHeap = M.empty

heapAlloc :: MonadState (Heap a) m => a -> m Handle
heapAlloc a = do
    h <- gets M.size
    modify (M.insert h a)
    pure h

heapPut :: (MonadState (Heap a) m, MonadPlus m) => Handle -> a -> m ()
heapPut h a = gets (M.member h) >>= \case
    True  -> modify (M.insert h a)
    False -> empty

heapGet :: (MonadState (Heap a) m, MonadPlus m) => Handle -> m a
heapGet h = gets (M.lookup h) >>= \case
    Just a  -> pure a
    Nothing -> empty

ex :: StateT (Heap Int) Maybe Int
ex = do
    x <- heapAlloc 1
    heapPut x 3
    heapGet x