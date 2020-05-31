module Data.Sexp

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

showSep : String -> List String -> String
showSep sep xs = showSepGo True xs "" where
  showSepGo : Bool -> List String -> String -> String
  showSepGo first [] acc = acc
  showSepGo first (x::xs) acc = if first then showSepGo False xs (acc ++ x)
                                         else showSepGo False xs (acc ++ " " ++ x)

export
Show Sexp where
  show (SAtom s) = s
  show (SList xs) = "(" ++ (showSep " " (assert_total $ map show xs)) ++ ")"
