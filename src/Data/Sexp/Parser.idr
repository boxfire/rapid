module Data.Sexp.Parser

import Text.Parser

import Data.Sexp
import Data.Sexp.Lexer

Parser : Type -> Type -> Type
Parser tok a = Grammar tok True a

exact : Eq a => Show a => a -> Parser a ()
exact t = terminal (show t) f where
  f : (WithBounds a) -> Maybe ()
  f x = if (val x) == t then Just () else Nothing

ignoreComment : Parser Token ()
ignoreComment = terminal "comment" f where
  f : WithBounds Token -> Maybe ()
  f (MkBounded (Comment _) _ _ _ _ _) = Just ()
  f _ = Nothing

mutual
  oneSexp : Parser Token Sexp
  oneSexp = (many ignoreComment) *> (parseSAtom <|> parseSList)

  parseSList : Parser Token Sexp
  parseSList = do
    content <- ((exact LParen) `seq` \_ => many oneSexp <* (exact RParen))
    pure $ SList content

  parseSAtom : Parser Token Sexp
  parseSAtom = terminal "atom" isAtom where
    isAtom : WithBounds Token -> Maybe Sexp
    isAtom (MkBounded (Atom a) _ _ _ _ _) = Just $ SAtom a
    isAtom (MkBounded (QuotedAtom a) _ _ _ _ _) = Just $ SAtom a
    isAtom _ = Nothing

sexpMain : Parser Token (List Sexp)
sexpMain = some oneSexp

export
parseSexp : List (WithBounds Token) -> Either (String) (List Sexp)
parseSexp toks = case parse sexpMain toks of
                      Left (Error s remaining) => Left $ "parse error: \"" ++ s ++ "\"\nremaining tokens: " ++ (show remaining)
                      Right (e, toks) => Right e
