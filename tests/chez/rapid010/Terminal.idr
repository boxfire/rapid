import Libraries.Utils.Term

main : IO ()
main = do
  setupTerm
  putStrLn $ "cols:  " ++ show !(getTermCols)
  putStrLn $ "lines: " ++ show !(getTermLines)
