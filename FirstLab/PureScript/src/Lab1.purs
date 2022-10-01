module Lab1
  (test)
  where

import Prelude

import Data.List (List(..))
import Effect (Effect)
import Effect.Console (log)

singleton :: forall a. a -> List a
singleton a = Cons a Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc lst a = case lst of
    Nil -> Cons a Nil
    Cons h t -> Cons h (snoc t a)

length :: forall a. List a -> Int
length a = case a of
    Nil -> 0
    Cons _ t -> 1 + length t

test::Effect Unit
test = do
  log $ show $ singleton "hi"
  log $ show $ null Nil
  log $ show $ null (Cons "notNull" Nil)
  log $ show $ snoc (Cons "hi" Nil) "bro"
  log $ show $ length (snoc (snoc (snoc (snoc (Cons "hi" Nil) "bro") "how") "are") "you")
