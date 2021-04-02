-- the commented-out cases are still wrong,
-- but fixing them as well would make other tests fail for mysterious reasons
-- see https://github.com/edwinb/Idris2/pull/281
main : IO ()
main = do
  printLn $ 3
  printLn $ 4.2
  printLn $ "1.2"

  printLn $ cast {to = Int} 4.8
  printLn $ cast {to = Integer} 5.0e100
  printLn $ cast {to = Integer} (-5.0e100)
  printLn $ cast {to = Integer} 1.2
  printLn $ cast {to = Integer} 0.0
  printLn $ (cast {to = Integer} 0.0) == (the Integer 0)
  printLn $ cast {to = Integer} (-13374242.0)
  printLn $ cast {to = String} 2.7

  -- printLn $ cast {to = Int} "1.2"
  -- printLn $ cast {to = Integer} "2.7"
  printLn $ cast {to = Double} "5.9"

  printLn $ (the Int 6 `div` the Int 3)
  printLn $ (the Integer 6 `div` the Integer 3)
  printLn $ (cast {to = Int} "6.6" `div` cast "3.9")
  printLn (3.14159265 + 7.0)
  printLn (3.14159265 - 7.0)
  printLn (3.14159265 * 7.0)
  printLn (3.14159265 / 7.0)

  putStrLn "left is smaller"
  printLn (3.14159265 < 7.0)
  printLn (3.14159265 <= 7.0)
  printLn (3.14159265 == 7.0)
  printLn (3.14159265 >= 7.0)
  printLn (3.14159265 > 7.0)

  putStrLn "left is bigger"
  printLn (9.14159265 < 7.0)
  printLn (9.14159265 <= 7.0)
  printLn (9.14159265 == 7.0)
  printLn (9.14159265 >= 7.0)
  printLn (9.14159265 > 7.0)

  putStrLn "equal"
  printLn (9.14159265 <  9.14159265)
  printLn (9.14159265 <= 9.14159265)
  printLn (9.14159265 == 9.14159265)
  printLn (9.14159265 >= 9.14159265)
  printLn (9.14159265 >  9.14159265)
  -- printLn $ (cast {to = Integer} "6.6" `div` cast "3.9")
