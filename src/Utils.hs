{-| Utility funcitons used in this Library. They are from extra and either package.

extra: http://hackage.haskell.org/package/extra-1.6.14/docs/Control-Monad-Extra.html

either: http://hackage.haskell.org/package/either-5.0.1
-}

module Utils
    ( eitherM
    , maybeM
    , fromMaybeM
    , whenRight
    , whenJust
    ) where

import           RIO

--------------------------------------------------------------------------------
-- Helper function
--------------------------------------------------------------------------------

-- | Monadic 'maybe'
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic 'fromMaybe'
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n return

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

-- | The 'whenRight' function takes an 'Either' value and a function which returns a monad.
-- The monad is only executed when the given argument takes the form @'Right' _@, otherwise
-- it does nothing.
whenRight :: Applicative m => Either a b -> (b -> m ()) -> m ()
whenRight (Right x) f = f x
whenRight _         _ = pure ()

-- | Monadic generalisation of 'either'.
eitherM :: Monad m => (a -> m c) -> (b -> m c) -> m (Either a b) -> m c
eitherM l r x = either l r =<< x
