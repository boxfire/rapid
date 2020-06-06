module Main

getAnswer : String -> String
getAnswer "four" = "eight"
getAnswer "two" = "four"
getAnswer "one" = "two"
getAnswer "xx" = "XXX danger XXX"
getAnswer "" = "empty"
getAnswer _ = "unknown"

getAnswer' : String -> String
getAnswer' x = if (prim__strHead x) == '-' then "minus" else "plus"

main : IO ()
main = do
  putStrLn (getAnswer' "-")
  putStrLn (getAnswer' "se")
  putStrLn (getAnswer "xxx")
  putStrLn (getAnswer "se")
  putStrLn (getAnswer "")
  putStrLn (getAnswer "four")
  putStrLn (getAnswer "two")
  putStrLn (getAnswer "one")
  putStrLn (getAnswer "two")
  putStrLn (getAnswer "xxx")
  putStrLn (getAnswer "xx")
  putStrLn (getAnswer "x")
  putStrLn (getAnswer "seven")
