
module System.Console.YAOP.Selector where

-- | Dummy selector, selects nothing. Useful for some @--help@ options.
dummy :: Monad m => (() -> m a) -> b -> m b
dummy f t = f () >> return t

-- | Monadic action over the first element, useful as selector.
firstM :: Monad m => (t -> m t1) -> (t, t2) -> m (t1, t2)
firstM  f (x,y) = f x >>= \x' -> return (x', y)

-- | Monadic action over the second element, useful as selector.
secondM :: Monad m => (t -> m t2) -> (t1, t) -> m (t1, t2)
secondM f (x,y) = f y >>= \y' -> return (x, y')
