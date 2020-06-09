module Main

import System.File

main : IO ()
main = do
  fr <- openFile "rapid003_testfile.txt" WriteTruncate
  case fr of
       Right f => do
         putStrLn "file opened"
         closeFile f
         putStrLn "file closed"
         fr2 <- openFile "/thisShouldNotWork/invalidPath/rapid003_testfile.txt" WriteTruncate
         case fr2 of
              Right f => do putStrLn "file2 opened"
                            closeFile f
                            putStrLn "file2 closed"
              Left err => putStrLn ("error opening file2: " ++ show err)
       Left err => do
         putStrLn ("error opening file: " ++ show err)
