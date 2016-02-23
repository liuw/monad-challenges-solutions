{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c

-- link :: Maybe a -> (a -> Maybe b) -> Maybe b
-- yLink :: Maybe a -> Maybe b -> (a -> b -> c) ->  Maybe c
         

-- f :: m a -> (a -> m b) -> m b
-- g :: (a -> b -> c) -> m a -> m b -> m c

-- Back to set1 ...

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing   = "Nothing"
  show (Just a)  = "Just " ++ show a

instance Monad Maybe where
    return a = Just a
    bind (Just a) f = f a
    bind Nothing _ = Nothing
    

instance Monad [] where
    return a = [a]
    bind (a:as) f = f a ++  bind as f
    bind [] _ = []

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
     return a = Gen (\s -> (a, s))
     bind ga f = Gen (\s -> let (a, s') = runGen ga s in runGen (f a) s')

evalGen :: Gen a -> Seed -> a
evalGen ga s = fst $ runGen ga s

-- rewrite repRandom in set1.hs as sequence
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (g:gs) = g `bind` (\a -> sequence gs `bind` (\b -> return $ a:b))

-- repRandom :: [Gen a] -> Gen [a]
-- repRandom (g:gs) = \s -> let (r , s')  = g s in
--                          let (r', s'') = repRandom gs s' in
--                          (r:r',s'')
-- repRandom [] = \s -> ([],s)

-- rewrite generalB2 in set1.hs as liftM2
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma `bind` (\a -> mb `bind` (\b -> return $ f a b))

-- rewrite chain in set2.hs as (=<<)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
-- (=<<) f ma = ma `bind` (\a -> f a)
(=<<) = flip bind


-- rewrite combine in set2.hs as join
-- I seemed to have forgotten to implement that one, but the type signature
-- seems to be
-- combine :: Maybe (Maybe a) -> Maybe a
join :: Monad m => m (m a) -> m a
join mma = mma `bind` id

-- rewrite allCombs3 in set3.hs as liftM3
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma `bind` (\a -> mb `bind` (\b -> mc `bind` (\c -> return $ f a b c)))

-- rewrite combStep in set3.hs as ap
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = ma `bind` (\a -> mf `bind` (\f -> return $ f a))
