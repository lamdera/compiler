module Network.Status where

import Network.Info
import Lamdera

ips = do
  ns <- getNetworkInterfaces

  ns
    & fmap ipv4
    & filter (\v -> show v /= "0.0.0.0")
    & filter (\v -> show v /= "127.0.0.1")
    & pure
    -- & mapM_ (putStr . show)


showInterface n = name n ++ "\n"
               ++ "  IPv4: " ++ show (ipv4 n) ++ "\n"
               ++ "  IPv6: " ++ show (ipv6 n) ++ "\n"
               ++ "  MAC:  " ++ show (mac n) ++ "\n"
