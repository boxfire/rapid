module Main

import public Control.Monad.State

export
data CGBuffer : Type where
  MkCGBuf : Int -> List String -> CGBuffer
{-
export
data Codegen : Type -> Type where
  MkCodegen : (Int -> (Int, a)) -> Codegen a

export
Functor Codegen where
  map f (MkCodegen c) = MkCodegen (\(i) => let (ni, r) = (c i) in (ni, f r))

export
Applicative Codegen where
  pure x = MkCodegen (\i => (i,x))
  (<*>) (MkCodegen fa) (MkCodegen fb) = MkCodegen (\i =>
                                        let (i1, f1) = fa i
                                            (i2, r2) = fb i1 in
                                            (i2, f1 r2)
                                        )
export
Monad Codegen where
  (>>=) (MkCodegen fx) f = MkCodegen $ \i => let (i', r1) = fx i
                                                 (MkCodegen f') = f r1 in
                                                 f' i'

-}

public export
Codegen : Type -> Type
Codegen = State CGBuffer

emptyCG : CGBuffer
emptyCG = MkCGBuf 0 []

export
appendCode : String -> Codegen ()
appendCode c = modify $ \(MkCGBuf i l) => (MkCGBuf i (c::l))

export
getUnique : Codegen Int
getUnique = do
  (MkCGBuf i l) <- get
  put (MkCGBuf (i+1) l)
  pure i

export
runCodegen : Codegen () -> String
runCodegen r = let (MkCGBuf _ ls) = snd $ runState r emptyCG in
                    unlines $ reverse ls
