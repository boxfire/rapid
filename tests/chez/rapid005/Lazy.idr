module Main

double : Int -> Int
double x = 2 * x

triple : Int -> Int
triple x = 10 * x

multiply : Int -> Int -> Int
multiply x y = x * y

IntFun : Type
IntFun = (Int -> Int)

data TheAlt : Type where
  MyCon : IntFun -> Lazy IntFun -> TheAlt

runCon : TheAlt -> Int -> Int
runCon (MyCon a b) i = let x = a i in if x > 5 then x else (b i)

main : IO ()
main = do
  let obj = MyCon double (multiply 100)
  --let obj = MyCon double triple
  printLn $ runCon obj 1
  printLn $ runCon obj 2
  printLn $ runCon obj 3
