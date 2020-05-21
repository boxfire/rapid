module SexpParser

import Text.Parser

import Compiler.VMCode
import Data.Sexp
import SexpLexer

Parser : Type -> Type -> Type
Parser tok a = Grammar tok True a

exact : Token -> Parser Token ()
exact t = terminal (show t) f where
  f : Token -> Maybe ()
  f x = if x == t then Just () else Nothing

mutual
  oneSexp : Parser Token Sexp
  oneSexp = parseSAtom <|> parseSList

  parseSList : Parser Token Sexp
  parseSList = do
    content <- ((exact LParen) `seq` \_ => many oneSexp <* (exact RParen))
    pure $ SList content

  parseSAtom : Parser Token Sexp
  parseSAtom = terminal "atom" isAtom where
    isAtom : Token -> Maybe Sexp
    isAtom (Atom a) = Just $ SAtom a
    isAtom (QuotedAtom a) = Just $ SAtom a
    isAtom _ = Nothing

sexpMain : Parser Token (List Sexp)
sexpMain = some oneSexp

export
parseSexp : List Token -> Either (String) (List Sexp)
parseSexp toks = case parse sexpMain toks of
                      Left (Error s toks) => Left $ "parse error: " ++ s ++ (show toks)
                      Right (e, toks) => Right e
