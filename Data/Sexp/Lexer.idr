module Data.Sexp.Lexer

import Data.List
import Data.Maybe
import Data.Strings
import Text.Lexer

import Parser.Support

%default covering

public export
data Token = LParen
           | RParen
           | Atom String
           | QuotedAtom String
           | Comment String

export
Show Token where
  show LParen = "LParen"
  show RParen = "RParen"
  show (Atom s) = "Atom \"" ++ s ++ "\""
  show (QuotedAtom s) = "QAtom \"" ++ s ++ "\""
  show (Comment s) = "; " ++ s ++ "\n"

export
Eq Token where
  LParen == LParen = True
  RParen == RParen = True
  _ == _ = False

comment : Lexer
comment = many space <+> is ';' <+> many (pred (/= '\n')) <+> is '\n'

lparen : Lexer
lparen = is '(' <+> many space

rparen : Lexer
rparen = is ')' <+> many space

isAlphNum : Char -> Bool
isAlphNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

ident : Lexer
ident = pred (\c => c /= '"' && c /= ' ' && c /= '(' && c /= ')' && c /= '\n' && c /= ';')

atom : Lexer
atom = some ident <+> many space

quotedAtom : Lexer
quotedAtom = stringLit <+> many space

removeQuotes : String -> String
removeQuotes s = assert_total $ reverse $ strTail $ reverse $ strTail s

tokenMap : TokenMap Token
tokenMap = [
  (lparen, \_ => LParen),
  (rparen, \_ => RParen),
  (atom, Atom . trim),
  (quotedAtom, QuotedAtom . (fromMaybe "<error>" . escape) . removeQuotes . trim),
  (comment, Comment)
  ]

export
lexSexp : String -> Either (String) (List Token)
lexSexp s = let (toks, line, col, remainder) = lex tokenMap s in
                if remainder == "" then Right $ map tok toks
                                   else Left $ "remaining: " ++ remainder
