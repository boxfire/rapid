module NetTest

import Network.Socket

main : IO ()
main = do
  putStrLn "net"
  printLn $ toCode AF_UNIX
  Right sock <- socket AF_INET Stream 0
    | Left err => printLn err

  let addr = IPv4Addr 127 0 0 1

  0 <- bind sock (Just addr) 4077
    | err => putStrLn $ "bind error: " ++ show err

  0 <- listen sock
    | err => putStrLn $ "listen error: " ++ show err

  Right (conn, saddr) <- accept sock
    | Left err => putStrLn $ "accept error: " ++ show err

  pure ()
