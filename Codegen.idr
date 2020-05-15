module Main

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

export
runCodegen : Codegen a -> a
runCodegen (MkCodegen f) = snd $ f 0

export
getUnique : Codegen Int
getUnique = MkCodegen $ \i => (i+1, i)

idrAlloc : Codegen String
idrAlloc = do
  uni <- getUnique
  uni2 <- getUnique
  pure $ (show uni2) ++ "%v" ++ (show (the Int uni))
