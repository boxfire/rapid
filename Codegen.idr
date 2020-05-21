module Codegen

import public Control.Monad.State
import Data.List
import Data.Strings

ConstDef : Type
ConstDef = (String, String)

export
data CGBuffer : Type where
  MkCGBuf : Int -> List ConstDef -> List String -> CGBuffer

public export
Codegen : Type -> Type
Codegen = State CGBuffer

emptyCG : CGBuffer
emptyCG = MkCGBuf 0 [] []

export
appendCode : String -> Codegen ()
appendCode c = modify $ \(MkCGBuf i consts l) => (MkCGBuf i consts (c::l))

export
getUnique : Codegen Int
getUnique = do
  (MkCGBuf i c l) <- get
  put (MkCGBuf (i+1) c l)
  pure i

export
addConstant : String -> Codegen String
addConstant v = do
  ci <- getUnique
  let name = "@glob_c" ++ show ci
  (MkCGBuf i c l) <- get
  put (MkCGBuf i ((name, v)::c) l)
  pure name

export
runCodegen : Codegen () -> String
runCodegen r = let (MkCGBuf _ cs ls) = snd $ runState r emptyCG in
                    unlines $ (map (\(n,v) => n ++ " = " ++ v) $ reverse cs) ++ reverse ls
