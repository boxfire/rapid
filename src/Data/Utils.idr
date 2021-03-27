module Data.Utils

import Data.Vect

export
enumerateVect : {n : Nat} -> Vect n a -> Vect n (Int, a)
enumerateVect l = enumerate' 0 l where
  enumerate' : {n : Nat} -> Int -> Vect n a -> Vect n (Int, a)
  enumerate' _ [] = []
  enumerate' i (x::xs) = (i, x)::(enumerate' (i+1) xs)

