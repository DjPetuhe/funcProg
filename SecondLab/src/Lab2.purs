module Lab2
  (test)
  where

import Prelude

import Data.List (List(..), reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

infixr 6 Cons as :

--first task
findIndex :: forall a. (a -> Boolean) -> List a -> Maybe(Int)
findIndex p (x:xs) = if p x 
                     then Just(0)
                     else case findIndex p xs of
                          Just (n) -> Just(1 + n)
                          _ -> Nothing
findIndex _ _ = Nothing

--second task
findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe(Int)
findLastIndex p (x:Nil) = if p x then Just(0) else Nothing
findLastIndex p (x:xs) = case findLastIndex p xs of
                         Just(n) -> Just(n + 1)
                         _ -> if p x then Just(0) else Nothing
findLastIndex _ _ = Nothing

--third task
zip :: forall a b. List a -> List b -> List (Tuple a b)
zip (a:Nil) (b:Nil) = ((Tuple a b) : Nil)
zip (a:Nil) (b:_) = ((Tuple a b) : Nil)
zip (a:_) (b:Nil) = ((Tuple a b) : Nil)
zip (a:as) (b:bs) = ((Tuple a b) : (zip as bs))
zip _ _ = Nil

--fourth task
unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = (Tuple Nil Nil)
unzip ((Tuple a b):xs) = let (Tuple la lb) = unzip xs in (Tuple (a:la) (b:lb)) 

--fifth task
filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (x:xs) = if p x then x : (filter p xs) else filter p xs

--sixth task
tailFilter :: forall a. (a -> Boolean) -> List a -> List a
tailFilter = go Nil
  where
    go :: forall b. List b -> (b -> Boolean) -> List b -> List b
    go acc _ Nil = reverse acc
    go acc pd (x:xs) = if pd x then go (x : acc) pd xs else go acc pd xs 

--seventh task
take :: forall a. Int -> List a -> List a
take _ Nil = Nil
take s (x:xs)
   | s <= 0 = Nil
   | otherwise = x : (take (s - 1) xs)

--eights task
tailTake :: forall a. Int -> List a -> List a
tailTake = go Nil
  where
    go :: forall b. List b -> Int -> List b -> List b
    go acc _ Nil = reverse acc
    go acc s (x:xs)
         | s <= 0 = reverse acc
         | otherwise = go (x : acc) (s - 1) xs

-- predicates
isTen :: Int -> Boolean
isTen a
    | a == 10 = true
    | otherwise = false

isNegative :: Int -> Boolean
isNegative a
         | a < 0 = true
         | otherwise = false

--tests
test :: Effect Unit
test = do
    log $ "findIndex func:"
    log $ show $ findIndex isTen (5 : 7 : -2 : 10 : 3 : Nil)
    log $ show $ findIndex isNegative (5 : 7 : -2 : 10 : 3 : Nil)
    log $ "findLastIndex func:"
    log $ show $ findLastIndex isTen (10 : 10 : 0 : 6 : 3 : Nil)
    log $ show $ findLastIndex isNegative (4 : 7 : 0 : -2 : 3 : Nil)
    log $ "zip func:"
    log $ show $ zip (10 : Nil) (4 : 7 : 0 : 5 : Nil)
    log $ show $ zip (10 : 10 : 0 : 6 : 3 : Nil) (4 : 7 : 0 : -2 : 3 : Nil)
    log $ "unzip func:"
    log $ show $ unzip (zip (10 : 10 : 0 : 6 : 3 : Nil) (4 : 7 : 0 : -2 : 3 : Nil))
    log $ "filter func:"
    log $ show $ filter isNegative (5 : 2 :Nil)
    log $ show $ filter isNegative (2 : -1 : 6 : 8 : -6 : -7 : 0 : Nil)
    log $ "tailFilter func:"
    log $ show $ filter isNegative (5 : 2 :Nil)
    log $ show $ tailFilter isNegative (2 : -1 : 6 : 8 : -6 : -7 : 0 : Nil)
    log $ "take func:"
    log $ show $ take 5 (2 : -1 : Nil)
    log $ show $ take (-5) (1 : 2 : 3 : Nil)
    log $ show $ take 5 (2 : -1 : 6 : 8 : -6 : -7 : 0 : Nil)
    log $ "tailTake func:"
    log $ show $ tailTake 5 (2 : -1 : Nil)
    log $ show $ tailTake (-5) (1 : 2 : 3 : Nil)
    log $ show $ tailTake 5 (2 : -1 : 6 : 8 : -6 : -7 : 0 : Nil)
    