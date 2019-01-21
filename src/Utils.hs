module Utils
    ( maybeM
    , fromMaybeM
    , whenRight
    ) where

import RIO

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- http://hackage.haskell.org/package/extra-1.6.14/docs/Control-Monad-Extra.html

-- | Monadic 'maybe'
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic 'fromMaybe'
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n return

-- http://hackage.haskell.org/package/either-5.0.1

-- | The 'whenRight' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @'Right' _@, otherwise
-- it does nothing.
whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _         _ = pure ()
