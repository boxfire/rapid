arg1 : Int -> String
arg1 a1 = "arg1: " ++ show a1

arg2 : Int -> Int -> String
arg2 a1 a2 = "arg2: " ++ show a1 ++ ", " ++ show a2

arg3 : Int -> Int -> Int -> String
arg3 a1 a2 a3 = "arg3: " ++ show a1 ++ ", " ++ show a2 ++ ", " ++ show a3

arg4 : Int -> Int -> Int -> Int -> String
arg4 a1 a2 a3 a4 = "arg4: " ++ show a1 ++ ", " ++ show a2 ++ ", " ++ show a3 ++ ", " ++ show a4

arg5 : Int -> Int -> Int -> Int -> Int -> String
arg5 a1 a2 a3 a4 a5 = "arg5: " ++ show a1 ++ ", " ++ show a2 ++ ", " ++ show a3 ++ ", " ++ show a4 ++ ", " ++ show a5

main : IO ()
main = do
  traverse_ putStrLn (map arg1 [13, 17])
  traverse_ putStrLn (map (arg2 5) [13, 17])
  traverse_ putStrLn (map (arg3 5 19) [13, 17])
  traverse_ putStrLn (map (arg4 5 19 23) [13, 17])
  traverse_ putStrLn (map (arg5 5 19 23 41) [13, 17])
