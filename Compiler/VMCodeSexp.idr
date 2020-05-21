module Compiler.VMCodeSexp

import Data.Either
import Data.List
import Data.Maybe
import Data.Strings

import Codegen
import Compiler.VMCode
import Core.TT
import Data.Sexp
import Utils.Hex

public export
ToSexp String where
  toSexp n = SAtom ("\"" ++ (show n) ++ "\"")

export
ToSexp Name where
  toSexp (MkName n) = SAtom ("\"" ++ (show n) ++ "\"")

ToSexp Reg where
  toSexp RVal = SAtom "RVAL"
  toSexp (Loc i) = SAtom ("v" ++ show i)
  toSexp Discard = SAtom "DISCARD"

shelper : ToSexp a => String -> List a -> Sexp
shelper s xs = SList ([SAtom s] ++ map toSexp xs)

public export
ToSexp Constant where
  toSexp c = SAtom $ show c

public export
ToSexp VMInst where
  toSexp (DECLARE r) = SList [SAtom "DECLARE", toSexp r]
  toSexp START = SList [SAtom "START"]
  toSexp (ASSIGN d s) = shelper "ASSIGN" [d, s]
  toSexp (MKCON reg tag args) = SList $ [SAtom "MKCON", toSexp reg, SAtom $ show tag, SList $ map toSexp args]
  toSexp (MKCLOSURE reg n missing args) = SList $ [SAtom "MKCLOSURE", toSexp reg, toSexp n, SAtom $ show missing, SList (map toSexp args)]
  toSexp (MKCONSTANT reg const) = SList $ [SAtom "MKCONSTANT", toSexp reg, toSexp const]
  toSexp (CALL reg isTail n args) = SList $ [SAtom "CALL", toSexp reg, SAtom $ show isTail, toSexp n, SList $ map toSexp args]
  toSexp (OP reg op args) = SList $ [SAtom "OP", toSexp reg, SAtom $ show op] ++ map toSexp (toList args)
  toSexp (APPLY reg f a) = shelper "APPLY" [reg, f, a]
  toSexp (CASE reg alts def) = SList ([SAtom "CASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Int, List VMInst) -> Sexp
    altToSexp (i, insts) = SList [SAtom $ show i, SList $ assert_total $ map toSexp insts]
  toSexp (CONSTCASE reg alts def) = SList ([SAtom "CONSTCASE", toSexp reg, defaultCase def] ++ (map altToSexp alts)) where
    defaultCase : Maybe (List VMInst) -> Sexp
    defaultCase def = case def of
                           Nothing => SList [SAtom "nodefault"]
                           Just insts => SList [SAtom "default", SList $ assert_total $ map toSexp insts]

    altToSexp : (Constant, List VMInst) -> Sexp
    altToSexp (c, insts) = SList [toSexp c, SList $ assert_total $ map toSexp insts]
  toSexp (PROJECT reg val pos) = SList [SAtom "PROJECT", toSexp reg, toSexp val, SAtom $ show pos]
  toSexp u = SList [SAtom "not-implemented", SAtom ("\"" ++ (assert_total $ show u) ++ "\"")]
  {-toSexp ASSIGN = SList -}


export
FromSexp Name where
  fromSexp (SAtom s) = Right $ MkName s
  fromSexp _ = Left $ "not a name"

export
FromSexp Bool where
  fromSexp (SAtom "True") = Right $ True
  fromSexp (SAtom "False") = Right $ False
  fromSexp _ = Left $ "invalid Bool"

FromSexp Reg where
  fromSexp (SAtom "RVAL") = Right RVal
  fromSexp (SAtom "DISCARD") = Right Discard
  fromSexp (SAtom "") = Left "invalid reg: \"\""
  fromSexp (SAtom s) = Right (Loc $ cast $ assert_total $ strTail s)
  fromSexp s = Left ("invalid reg: " ++ show s)

export
FromSexp Constant where
  fromSexp (SList [SAtom "I", SAtom i]) = Right $ I $ cast i
  fromSexp (SList [SAtom "BI", SAtom i]) = Right $ BI $ cast i
  fromSexp (SList [SAtom "Str", SAtom s]) = Right $ Str s
  fromSexp (SList [SAtom "Ch", SAtom c]) = Right $ Ch $ assert_total $ strIndex c 0
  fromSexp (SList [SAtom "Db", SAtom d]) = Right $ Db $ cast d
  fromSexp (SList [SAtom "%World"]) = Right $ WorldVal
  fromSexp s = Left $ "invalid constant: " ++ show s

collectFromSexp : FromSexp a => List Sexp -> Either String (List a)
{-collectFromSexp s = Right $ rights $ map fromSexp s-}
{-collectFromSexp s = sequence $ map fromSexp s-}
collectFromSexp s = traverse fromSexp s

export
FromSexp VMInst where
  fromSexp (SList [SAtom "DECLARE", r]) = fromSexp r >>= pure . DECLARE
  fromSexp (SList [SAtom "START"]) = Right START
  fromSexp (SList [SAtom "ASSIGN", d, s]) = do
    pd <- fromSexp d
    ps <- fromSexp s
    pure $ ASSIGN pd ps
  fromSexp (SList [SAtom "MKCON", regS, SAtom tagS, SList argsS]) = do
    reg <- fromSexp regS
    let tag = cast tagS
    args <- collectFromSexp argsS
    pure $ MKCON reg tag args
  fromSexp (SList [SAtom "MKCLOSURE", regS, nameS, SAtom missingStr, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    args <- collectFromSexp argsS
    pure $ MKCLOSURE reg name (stringToNatOrZ missingStr) args
  fromSexp (SList [SAtom "MKCONSTANT", regS, constS]) = do
    reg <- fromSexp regS
    const <- fromSexp constS
    pure $ MKCONSTANT reg const
  fromSexp (SList [SAtom "CALL", regS, tailS, nameS, SList argsS]) = do
    reg <- fromSexp regS
    name <- fromSexp nameS
    tail <- fromSexp tailS
    args <- collectFromSexp argsS
    pure $ CALL reg tail name args
  fromSexp (SList ((SAtom "OP")::regS::(SAtom name)::argsS)) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    pure $ OP reg name args
  fromSexp (SList [SAtom "APPLY", regS, fS, argS]) = do
    reg <- fromSexp regS
    f <- fromSexp fS
    arg <- fromSexp argS
    pure $ APPLY reg f arg
  fromSexp (SList ((SAtom "CONSTCASE")::regS::defaultS::altsS)) =
    do reg <- fromSexp regS
       defaultV <- assert_total $ readDefault defaultS
       pure $ CONSTCASE reg (assert_total $ rights $ map readAlt altsS) defaultV
    where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
        readAlt : Sexp -> Either String (Constant, (List VMInst))
        readAlt (SList [SAtom c, SList is]) = do
          insts <- collectFromSexp is
          pure $ (I $ cast c, insts)
        readAlt _ = Left $ "error in alt"
  fromSexp (SList ((SAtom "CASE")::regS::defaultS::altsS)) =
    (do
      reg <- fromSexp regS
      defaultV <- assert_total $ readDefault defaultS
      pure $ CASE reg [] defaultV
      )
      where
        readDefault : Sexp -> Either String (Maybe (List VMInst))
        readDefault (SList [SAtom "nodefault"]) = Right Nothing
        readDefault (SList [SAtom "default", SList is]) = do
          insts <- collectFromSexp is
          pure $ Just insts
        readDefault _ = Right Nothing --Left "invalid default"
  fromSexp (SList ((SAtom "EXTPRIM")::regS::(SAtom name)::(SList argsS)::[])) = do
    reg <- fromSexp regS
    args <- collectFromSexp argsS
    pure $ EXTPRIM reg name args
  fromSexp sexp = Left $ "vminst not impl" ++ show sexp

public export
ToSexp VMDef where
  toSexp (MkVMFun args insts) = SList $ [SAtom "fun", SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (MkVMError insts) = SList $ [SAtom "error", SList $ map toSexp insts]

public export
ToSexp (String, VMDef) where
  toSexp (n, (MkVMFun args insts)) = SList $ [SAtom "defun", toSexp n, SList $ map (\i => SAtom $ "v" ++ show i) args, SList $ map toSexp insts]
  toSexp (n, (MkVMError insts)) = SList $ [SAtom "deferr", toSexp n, SList $ map toSexp insts]

{-listFromSexp : {FromSexp a} => FromSexp (List a)-}
{-listFromSexp {fa} =-}
  {-let mapped = map fromSexp s-}

{-export-}
{-FromSexp a => FromSexp (List a) where-}
  {-fromSexp (SAtom _) = Left "not a list"-}
  {-fromSexp s = let mapped = map fromSexp s in-}
                   {-?fmmm-}

export
FromSexp (String, VMDef) where
  fromSexp (SList [SAtom "defun", SAtom n, SList args, SList insts]) = do
    fArgs <- collectFromSexp args
    fInsts <- collectFromSexp insts
    pure (n, MkVMFun fArgs fInsts)
  fromSexp _ = Left "invalid vmdef"


