module Data.Sexp

import Data.Strings

%default total

public export
data Sexp : Type where
  SAtom : String -> Sexp
  SList : List Sexp -> Sexp

public export
interface ToSexp a where
  toSexp : a -> Sexp

public export
interface FromSexp a where
  fromSexp : Sexp -> Either String a

isSafe : String -> Bool
isSafe "" = False
isSafe s = let l = prim__strLength s in
               if l > 20 then False else
               go s l 0 where
                 safeChar : Char -> Bool
                 safeChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c == '-'

                 go : String -> Int -> Int -> Bool
                 go s l i = if i >= l then True
                                      else safeChar (assert_total $ strIndex s i) && (assert_total $ go s l (i+1))

showSep : String -> List String -> String
showSep sep xs = showSepGo True xs "" where
  showSepGo : Bool -> List String -> String -> String
  showSepGo first [] acc = acc
  showSepGo first (x::xs) acc = if first then showSepGo False xs (acc ++ x)
                                         else showSepGo False xs (acc ++ " " ++ x)

export
Show Sexp where
  show (SAtom s) = if isSafe s then s else (show s)
  show (SList xs) = "(" ++ (showSep " " (assert_total $ map show xs)) ++ ")"

export
unAtom : (errmsg : String) -> Sexp -> Either String String
unAtom _ (SAtom s) = Right s
unAtom m v = Left $ "expected an atom as " ++ m ++ ", got: " ++ show v
