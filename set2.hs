{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  show Nothing   = "Nothing"
  show (Just a)  = "Just " ++ show a


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay k ((a,b):xs) = if a == k then Just b else lookupMay k xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just (a / b)

maximum' :: (Ord a) => a -> [a] -> a
maximum' m [] = m
maximum' m (x:xs) = if m > x then maximum' m xs else maximum' x xs

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just (maximum' (xs!!0) (drop 1 xs))

minimum' :: (Ord a) => a -> [a] -> a
minimum' m [] = m
minimum' m (x:xs) = if m < x then minimum' m xs else minimum' x xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just (minimum' (xs!!0) (drop 1 xs))

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d s = case lookupMay s d of
                   Nothing -> Nothing
                   Just (xs) -> case tailMay xs of
                                  Nothing  -> Nothing
                                  Just xs' -> case maximumMay xs' of
                                                Nothing -> Nothing
                                                Just m -> case (headMay xs) of
                                                            Nothing -> Nothing
                                                            Just h -> divMay (fromIntegral m) (fromIntegral h)


chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain next x = case x of
            Nothing -> Nothing
            Just a -> next a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

-- queryGreek2 :: GreekData -> String -> Maybe Double
-- queryGreek2 d s = case (lookupMay s d `link` tailMay `link` maximumMay) of
--                     Nothing -> Nothing
--                     Just m -> (case (lookupMay s d `link` headMay) of
--                                  Nothing -> Nothing
--                                  Just h -> divMay (fromIntegral m) (fromIntegral h))

queryGreek2 d s = lookupMay s d `link` tailMay `link` maximumMay `link` (\m ->  (lookupMay s d `link` headMay) `link` (\h -> divMay (fromIntegral m) (fromIntegral h)))

queryGreek2' d s = lookupMay s d `link` (\e -> tailMay e `link` maximumMay `link` (\m -> headMay e `link` (\h -> divMay (fromIntegral m) (fromIntegral h))))


-- addMay :: Num a => a -> a -> Maybe a
-- addMay a b  = Just (a + b)

-- addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
-- addSalaries l sa sb = lookupMay sa l `link` (\ia -> lookupMay sb l `link` (\ib -> addMay ia ib))

-- addSalaries l a b = case lookupMay a l of
--                       Nothing -> Nothing
--                       Just sa -> case lookupMay b l of
--                                    Nothing -> Nothing
--                                    Just sb -> Just (sa + sb)

{-

[(a,b)] -> a -> a -> Maybe b

pattern
:: a -> a -> Maybe b

yChain chains above pattern together, so the first argument should be

(a -> a -> Maybe b)

What next? yChain should expect to see Maybe a from "upstream" and
then strip the Maybe away to apply that to first argument

(a -> a -> Maybe b) -> Maybe a

And of course it continues to expect another Maybe a so that it can,
again, strip Maybe and apply a to first argument.

(a -> a -> Maybe b) -> Maybe a -> Maybe a

Finally it yields Maybe b

(a -> a -> Maybe b) -> Maybe a -> Maybe a -> Maybe b

One thing I missed, it can be more generalised to be:

(a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c

-}

-- yChain :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
-- yChain f ma1 ma2 = case ma1 of
--                      Nothing -> Nothing
--                      Just a1 -> case ma2 of
--                                   Nothing -> Nothing
--                                   Just a2 -> f a1 a2
-- yChain f ma1 ma2 = ma1 `link` (\a1 -> ma2 `link` \a2 -> f a1 a2)

-- addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
-- addSalaries2 l a b = yChain addMay (lookupMay a l) (lookupMay b l)


{-

Well the challenge says the operator doesn't fail, so the correct answer is

(a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

See the above failed attempt for detailed analysis.

-}

yChain :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yChain f ma1 ma2 = ma1 `link` (\a1 -> ma2 `link` \a2 -> mkMaybe $ f a1 a2)

mkMaybe :: a -> Maybe a
mkMaybe x = Just x

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 l a b = yChain (+) (lookupMay a l) (lookupMay b l)


-- Use tailMay
tailProd :: Num a => [a] -> Maybe a
tailProd xs = tailMay xs `link` (\xs' -> Just $ product xs')

tailSum :: Num a => [a] -> Maybe a
tailSum xs = tailMay xs `link` (\xs' -> Just $ sum xs')

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f ma = ma `link` (\v -> Just $ f v)

tailProd' :: Num a => [a] -> Maybe a
tailProd' xs = transMaybe product (tailMay xs)

tailSum' :: Num a => [a] -> Maybe a
tailSum' xs = transMaybe sum (tailMay xs)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = transMaybe maximumMay (tailMay xs)

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = transMaybe minimumMay (tailMay xs)
