module Ops where

(+++) :: Int -> Int -> Int
k +++ b = k + b

class a ::: b where
  a :: a -> b
  (***) :: a -> b -> ()
