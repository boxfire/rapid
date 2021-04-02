||| Manual test:
||| since times (naturally) vary between runs, this can only be verified by manually inspecting the output
import System.Clock

main : IO ()
main = do
  printLn !(clockTime UTC)
  printLn !(clockTime Monotonic)
  printLn !(clockTime Duration)
  printLn !(clockTime Process)
  printLn !(clockTime Thread)
  printLn !(clockTime GCCPU)
  printLn !(clockTime GCReal)
