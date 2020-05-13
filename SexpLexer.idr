module SexpLexer

{-import Data.Strings-}
import Text.Lexer

public export
data Token = LParen
           | RParen
           | Atom String
           | QuotedAtom String

export
Show Token where
  show LParen = "LParen"
  show RParen = "RParen"
  show (Atom s) = "Atom \"" ++ s ++ "\""
  show (QuotedAtom s) = "QAtom \"" ++ s ++ "\""

export
Eq Token where
  LParen == LParen = True
  RParen == RParen = True
  _ == _ = False

lparen : Lexer
lparen = is '(' <+> many space

rparen : Lexer
rparen = is ')' <+> many space

isAlphNum : Char -> Bool
isAlphNum c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

ident : Lexer
{-ident = oneOf "{}:_-+*/%.<>[]|=" <|> (pred isAlphNum)-}
ident = pred (\c => c /= '"' && c /= ' ' && c /= '(' && c /= ')' && c /= '\n')

atom : Lexer
atom = some ident <+> many space

quotedAtom : Lexer
quotedAtom = is '"' <+> some (pred (\x => x /= '"')) <+> is '"' <+> many space

removeQuotes : String -> String
removeQuotes = assert_total . reverse . strTail . reverse . strTail

tokenMap : TokenMap Token
tokenMap = [
  (lparen, \_ => LParen),
  (rparen, \_ => RParen),
  (atom, Atom . trim),
  (quotedAtom, QuotedAtom . removeQuotes . trim)
  ]

export
lexSexp : String -> Either (String) (List Token)
lexSexp s = let (toks, line, col, remainder) = lex tokenMap s in
                if remainder == "" then Right $ map tok toks
                                   else Left $ "remaining: " ++ remainder

