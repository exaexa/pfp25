{-
 - Compile with:
 - ghc paintserver.hs -o paintserver -package network -package vector -package stm
 -}

module Main where

import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad
import Data.List
import qualified Data.Vector as V
import Network.Socket
import System.IO
import Text.Read

boardSize = (32, 32) :: (Int, Int)

boardDataSize = x * y
  where
    (x, y) = boardSize

data Pix
  = Transparent
  | Dark
  | Light
  deriving (Show, Read)

type Board = V.Vector Pix

newBoard = V.replicate boardDataSize Transparent

boardIdx ix iy = ix + x * iy
  where
    (x, _) = boardSize

boardXY idx = (idx `mod` x, idx `div` x)
  where
    (x, _) = boardSize

inBoard ix iy = ix >= 0 && ix < x && iy >= 0 && iy < y
  where
    (x, y) = boardSize

fieldChar Transparent = '.'
fieldChar Dark = 'o'
fieldChar Light = 'x'

boardPixel c ix iy b =
  if inBoard ix iy
    then b V.// [(idx, c)]
    else b
  where
    idx = boardIdx ix iy

boardPrint b =
  putStrLn "---" >>
  forM_
    [0 .. y - 1]
    (\i -> putStrLn $ map fieldChar $ V.toList $ V.slice (x * i) x b)
  where
    (x, y) = boardSize

data InMsg
  = DoPoll
  | DoPix Pix Int Int
  | DoTerminate

data OutMsg
  = SetBoard (V.Vector Pix)
  | SetPix Pix Int Int

data ServerCom =
  ServerCom
    { inChan :: Chan InMsg
    , outChan :: TChan OutMsg
    }

newServerCom = ServerCom <$> newChan <*> newBroadcastTChanIO

workerThread :: ServerCom -> Board -> IO ()
workerThread com board = do
  let continue b = workerThread com b
      broadcast b = atomically $ writeTChan (outChan com) b
  msg <- readChan (inChan com)
  case msg of
    DoPoll -> broadcast (SetBoard board) >> continue board
    DoPix p x y -> broadcast (SetPix p x y) >> continue (boardPixel p x y board)
    DoTerminate -> pure ()

main =
  withSocketsDo $ do
    com <- newServerCom
    worker <- forkIO $ workerThread com newBoard
    E.bracket open close $ mainLoop com
    writeChan (inChan com) DoTerminate
  where
    open = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      bind sock $ SockAddrInet 10042 0
      listen sock 10
      return sock

mainLoop com sock =
  forever $ do
    (c, _) <- accept sock
    forkIO $ E.bracket (setupConn c) hClose $ runConn com

setupConn c = do
  h <- socketToHandle c ReadWriteMode
  hSetBuffering h NoBuffering
  return h

runConn com h = do
  myChan <- atomically $ dupTChan (outChan com)
  let recvLoop = loop
        where
          loop = do
            cmd <- words . filter (>= ' ') <$> hGetLine h --the filter here removes the \r (and other stuff) sent by telnet
            case cmd of
              ["Poll"] -> do
                writeChan (inChan com) DoPoll
                loop
              [pix, p1, p2] -> do
                let input =
                      DoPix <$> readMaybe pix <*> readMaybe p1 <*> readMaybe p2
                maybe (hPutStrLn h "Error") (writeChan $ inChan com) input
                loop
              ["Quit"] -> pure ()
              [] -> loop
              _ -> do
                hPutStrLn h ("Error")
                loop
  let sendLoop =
        forever $
         do
          x <- atomically $ readTChan myChan
          case x of
            SetBoard v -> hPutStrLn h $ "Paper " ++ map fieldChar (V.toList v)
            SetPix p x y ->
              hPutStrLn h $ intercalate " " [show p, show x, show y]
  sender <- forkIO $ sendLoop
  recvLoop
  killThread sender
