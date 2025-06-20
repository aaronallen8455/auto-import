module Ops where

(+++) :: Int -> Int -> Int
k +++ b = k + b

data a ::: b = Mk
  { method :: a -> b
  , (***) :: a -> b -> ()
  }
