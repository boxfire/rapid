module Main

main : IO ()
main = do
  line <- getLine
  putStrLn $ "input was: " ++ show line
