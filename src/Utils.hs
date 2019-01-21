module Utils
    ( maybeM
    , fromMaybeM
    , whenRight
    ) where

import RIO

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Monadic maybe
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic fromMaybe
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n return

whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _         _ = pure ()
