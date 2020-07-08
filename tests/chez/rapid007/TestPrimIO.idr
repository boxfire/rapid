module Main

import System
import System.File

main : IO ()
main = do
  writeFile "temporarytestfile" "#!/usr/bin/env true\n"
  chmodRaw "temporarytestfile" 0o755
  the (IO ()) exitSuccess

  fPutStr stdout "to:stdout\n"
  fPutStr stderr "to:stderr\n"
  line <- getLine
  putStrLn $ "input was: " ++ show line
  putStrLn $ "please press any key to continue..."
  ch <- fGetChar stdin
  putStrLn $ "thanks for pressing " ++ show ch
  fourChars <- fGetChars stdin 4
  putStrLn $ "thanks for entering " ++ show fourChars
  rest <- fGetChars stdin 1024
  putStrLn $ "rest was: " ++ show rest
