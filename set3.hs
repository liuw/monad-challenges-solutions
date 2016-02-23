{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

iteratef :: [a -> b] -> [a] -> [b]
iteratef (f:fs) as' = (map f as') ++ (iteratef fs as')
iteratef [] _  = []

allPairs :: [a] -> [b] -> [(a,b)]
-- allPairs xs ys = [ (x,y) | x <- xs, y <- ys]
allPairs as bs = iteratef (iteratef [(,)] as) bs

data Card = Card Int String

instance Show Card where
    show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards is ss = iteratef (iteratef [Card] is) ss

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
-- allCombs f as bs = iteratef (iteratef [f] as) bs
allCombs f = iteratef . iteratef [f]

allPairs' = allCombs (,)
allCards' = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- allCombs3 f as bs cs = iteratef (iteratef (iteratef [f] as) bs) cs
-- allCombs3 f as bs = iteratef (allCombs f as bs)
-- allCombs3 f as bs cs = iteratef (iteratef (iteratef [f] as) bs) cs

iteratefv = flip iteratef
allCombs3 f as bs cs = iteratefv cs $ iteratefv bs $ iteratefv as [f]

-- x f as = iteratefv as [f]
-- xxx f as bs cs = iteratefv cs (iteratefv bs (iteratefv as [f]))

-- (f . g) x = f(g(x))


-- Heh, I figured it out before I was asked to do it?
combStep = iteratef
