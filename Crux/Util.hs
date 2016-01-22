{-# LANGUAGE LambdaCase #-}

module Crux.Util where

findFirstOf :: [a] -> (a -> Maybe b) -> Maybe b
findFirstOf [] _ = Nothing
findFirstOf (x:xs) f = case f x of
    Just y -> Just y
    Nothing -> findFirstOf xs f

findFirstOfM :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
findFirstOfM [] _ = return Nothing
findFirstOfM (x:xs) f = do
    f x >>= \case
        Just y -> return $ Just y
        Nothing -> findFirstOfM xs f
