module Main

import Data.Strings

-- Test, that Scheme-dependent builtins are correctly blacklisted/overridden

main : IO ()
main = do
  putStrLn $ fastPack ['a', 'b', 'c']
  putStrLn $ fastAppend ["first part", "|", "", "|", "END of the string"]
