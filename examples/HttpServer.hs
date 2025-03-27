import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Network.Socket

main =
  withSocketsDo $ do
    counter <- newMVar 0
    bracket open close (serverLoop counter)
  where
    open = do
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock $ SockAddrInet 8080 0
      setCloseOnExecIfNeeded $ fdSocket sock
      listen sock 10
      return sock
    serverLoop counter sock =
      forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "client: " ++ show peer
        void $ forkFinally  (talk counter conn)
                            (\_ -> close conn)
    talk counter conn = do
      msg <- words <$> recv conn 1024 -- String!
      putStrLn $ "Request: " ++ show msg
      case msg of
        "GET":_ -> do
          ctr <- takeMVar counter
          putMVar counter (ctr + 1)
          send conn $
            "HTTP/1.0 200 OK\n" ++
            "Content-type: text/plain\n\n" ++
            "Hello, " ++ show ctr
        _ -> send conn "HTTP 400 bad request\n\n"
