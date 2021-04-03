module Main

import System
import System.File

main : IO ()
main = do
  efh <- openFile "temporarytestfile" Read
  case efh of
    Right fh => do
      printLn $ "mtime: " ++ show !(fileModifiedTime fh)
      printLn $ "atime: " ++ show !(fileAccessTime fh)
      printLn $ "ctime: " ++ show !(fileStatusTime fh)
      closeFile fh
    Left err => putStrLn $ "file error: " ++ show err

  ignore $ writeFile "temporarytestfile" "#!/usr/bin/env true\n"
  ignore $ chmodRaw "temporarytestfile" 0o755
  rc1 <- system "/usr/bin/true"
  rc2 <- system "ls -la ./temporarytestfile"
  printLn $ the (List Int) [rc1, rc2]

  path <- getEnv "PATH"
  putStrLn $ "$PATH = " ++ show path

  path <- getEnv "NOTEXISTINGENVVAR"
  putStrLn $ "$NOTEXISTINGENVVAR = " ++ show path

  the (IO ()) exitSuccess

  ignore $ fPutStr stdout "to:stdout\n"
  ignore $ fPutStr stderr "to:stderr\n"
  line <- getLine
  putStrLn $ "input was: " ++ show line
  putStrLn $ "please press any key to continue..."
  ch <- fGetChar stdin
  putStrLn $ "thanks for pressing " ++ show ch
  fourChars <- fGetChars stdin 4
  putStrLn $ "thanks for entering " ++ show fourChars
  rest <- fGetChars stdin 1024
  putStrLn $ "rest was: " ++ show rest
