module Main

import System.File

main : IO ()
main = do
  fPutStr stdout "to:stdout\n"
  fPutStr stderr "to:stderr\n"
  line <- getLine
  putStrLn $ "input was: " ++ show line
