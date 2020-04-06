{-# LANGUAGE ScopedTypeVariables #-}

module Internal.Safe (ignoringError) where

import Control.Exception (catch)

ignoringError :: IO a -> IO (Maybe a)
ignoringError action =  (Just <$> action) `catch` (\(_:: IOError) -> pure $ Nothing)
