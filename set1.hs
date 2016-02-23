{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- rand :: Seed -> (Integer, Seed)
-- mkSeed :: Integer -> Seed

fiveRands :: [Integer]
fiveRands = let (r1,s1) = (rand $ mkSeed 1) in
            let (r2,s2) = (rand $ s1) in
            let (r3,s3) = (rand $ s2) in
            let (r4,s4) = (rand $ s3) in
            let (r5,_ ) = (rand $ s4) in
            [r1, r2, r3, r4, r5]

-- randLetter :: Seed -> (Char, Seed)
randLetter :: Gen Char -- see below
randLetter seed = let (i,s) = rand seed in
                  (toLetter i, s)

randString3 :: String
randString3 = let (c1,s1) = (randLetter $ mkSeed 1) in
              let (c2,s2) = (randLetter $ s1) in
              let (c3,_ ) = (randLetter $ s2) in
              [c1, c2, c3]



type Gen a = Seed -> (a, Seed)


-- randEven :: Gen Integer
-- randEven s = let (i,s1) = (rand s) in
--              (i*2, s1)

-- randOdd :: Gen Integer
-- randOdd s = let (i,s1) = (rand s) in
--              (i*2+1, s1)

-- randTen :: Gen Integer
-- randTen s = let (i,s1) = (rand s) in
--              (i*10, s1)

-- generalA :: (Integer -> Integer) -> Gen Integer
-- generalA f s = let (i,s1) = rand s in
--                (f i, s1)
-- randEven :: Gen Integer
-- randEven = generalA (\x -> x * 2)

-- randOdd :: Gen Integer
-- randOdd = generalA (\x -> x * 2 + 1)

-- randTen :: Gen Integer
-- randTen = generalA (\x -> x * 10)

-- this is more or less the right generalA 
trans :: (a -> a) -> Gen a -> Gen a
trans f g = \s -> let (r,s1) = g s in (f r, s1)

randEven :: Gen Integer
randEven = trans (\x -> x * 2) rand


randPair :: Gen (Char, Integer)
randPair s = let (c,s1) = randLetter s in
             let (i,s2)  = rand s1 in
             ((c,i),s2)

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb = \s -> let (a,s1) = ga s in
                          let (b,s2) = gb s1 in
                          ((a,b),s2)
randPair' :: Gen (Char, Integer)
randPair' = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb = \s -> let (a,s1) = ga s in
                         let (b,s2) = gb s1 in
                         (f a b, s2)

randPair'' :: Gen (Char, Integer)
randPair'' = generalB (,) randLetter rand


repRandom :: [Gen a] -> Gen [a]
repRandom (g:gs) = \s -> let (r , s')  = g s in
                         let (r', s'') = repRandom gs s' in
                         (r:r',s'')
repRandom [] = \s -> ([],s)

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f = \s -> let (r,s') = ga s in
                    f r s'

mkGen :: a -> Gen a
mkGen a = \s -> (a, s)

-- Comming back from set4

-- Use genTwo to implement generalB, get rid of threading of state.
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = ga `genTwo` (\a -> gb `genTwo` \b -> mkGen $ f a b)

generalA :: (a -> b) -> Gen a -> Gen b
-- generalA f ga = \s -> let (a, s') = ga s in (f a, s')
generalA f ga = genTwo ga (\a -> mkGen $ f a)

-- I actually had a hard time convincing myself the implementation
-- using genTwo is correct. I just smashed things together until type
-- signatures match. But the randEven and randEven' seem to generate
-- the same results.

randEven' :: Gen Integer
randEven' = generalA (\x -> x * 2) rand




-- Reimplement repRandom with generalA, genTwo and mkGen


-- type Gen a = Seed -> (a, Seed)

-- generalA :: (a -> b) -> Gen a -> Gen b
-- generalA f ga = \s -> let (a, s') = ga s in (f a, s')

-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- genTwo ga f = \s -> let (r,s') = ga s in
--                     f r s'

-- mkGen :: a -> Gen a
-- mkGen a = \s -> (a, s)

-- repRandom :: [Gen a] -> Gen [a]
-- repRandom (g:gs) = \s -> let (r , s')  = g s in
--                          let (r', s'') = repRandom gs s' in
--                          (r:r',s'')
-- repRandom [] = \s -> ([],s)

repRandom2 :: [Gen a] -> Gen [a]

-- Again, the analysis is just very convoluted. The end result is that
-- there isn't a need to present the state variable at all. genTwo
-- handles that very well.
repRandom2 (g:gs) = g `genTwo` (\a -> repRandom2 gs `genTwo`  (\b -> mkGen $ a : b))
repRandom2 [] = mkGen []
