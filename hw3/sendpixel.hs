{-
 - Compile with:
 - ghc sendpixel.hs -o sendpixel -package network
 -}
module Main where

import System.IO
import Network.Socket

main =
  withSocketsDo $ do
    sock <- socket AF_INET Stream 0
    addr <-
      addrAddress . head <$>
      getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "10042")
    connect sock addr
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    hPutStrLn h "Dark 0 0"
    response <- hGetLine h
    putStrLn $ "Received this: " ++ response
    hPutStrLn h "Poll"
    response <- hGetLine h
    putStrLn $ "Received more: " ++ response
    hClose h
