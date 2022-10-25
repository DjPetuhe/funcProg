module Lab2
  (test)
  where

import Prelude

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

infixr 6 Cons as :

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe(Int)
findIndex p (x:xs) = if p x 
                     then Just(0)
                     else case findIndex p xs of
                          Just (n) -> Just(1 + n)
                          _ -> Nothing
findIndex _ _ = Nothing

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe(Int)
findLastIndex p (x:Nil) = if p x then Just(0) else Nothing
findLastIndex p (x:xs) = case findLastIndex p xs of
                         Just(n) -> Just(n + 1)
                         _ -> if p x then Just(0) else Nothing
findLastIndex _ _ = Nothing

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip (a:Nil) (b:Nil) = ((Tuple a b) : Nil)
zip (a:Nil) (b:_) = ((Tuple a b) : Nil)
zip (a:_) (b:Nil) = ((Tuple a b) : Nil)
zip (a:as) (b:bs) = ((Tuple a b) : (zip as bs))
zip _ _ = Nil

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = (Tuple Nil Nil)
unzip ((Tuple a b):xs) = let (Tuple la lb) = unzip xs in (Tuple (a:la) (b:lb)) 

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (x:xs) = if p x then x : (filter p xs) else filter p xs

-- predicates

isTen :: Int -> Boolean
isTen a
    | a == 10 = true
    | otherwise = false

isNegative :: Int -> Boolean
isNegative a
         | a < 0 = true
         | otherwise = false

test :: Effect Unit
test = do
    --first func (findIndex) test
    log $ show $ findIndex isTen (5 : 7 : -2 : 10 : 3 : Nil)
    log $ show $ findIndex isNegative (5 : 7 : -2 : 10 : 3 : Nil)
    --second func (findLastIndex) test
    log $ show $ findLastIndex isTen (10 : 10 : 0 : 6 : 3 : Nil)
    log $ show $ findLastIndex isNegative (4 : 7 : 0 : -2 : 3 : Nil)
    --third func (zip)
    log $ show $ zip (10 : Nil) (4 : 7 : 0 : 5 : Nil)
    log $ show $ zip (10 : 10 : 0 : 6 : 3 : Nil) (4 : 7 : 0 : -2 : 3 : Nil)
    --fourth func (unzip)
    log $ show $ unzip (zip (10 : 10 : 0 : 6 : 3 : Nil) (4 : 7 : 0 : -2 : 3 : Nil))
    --fifth func (filter)
    log $ show $ filter isNegative (2 : -1 : 6 : 8 : -6 : -7 : 0 : Nil)



