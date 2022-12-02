{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module NetUtil (createRoom, joinGame, waitForPlacement, sendPlacement ) where

import Game
import Data.Binary ( decode, encode, Binary(get, put) )
import Control.Monad (forever)
import Network.Socket 
import Network.Socket (getAddrInfo)
import qualified Control.Exception as EX
import Network.Socket.ByteString ( recv, send )
import Data.ByteString (toStrict, fromStrict)
import Brick.BChan ( writeBChan, BChan )


type R = Int
type C = Int
data Payload = Position C R deriving (Eq, Show)

instance Binary Payload where
  put (Position x y) = do
    put x
    put y

  get = do
    x <- get
    y <- get
    return $ Position x y


joinGame :: IO Socket
joinGame = do
    let hints = defaultHints { addrSocketType = Stream }
    addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "2333")
    skt <- openSocket addr 
    connect skt $ addrAddress addr
    return skt

createRoom :: IO Socket
createRoom = do
    let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
    addr <- head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "2333")
    skt <- openSocket addr
    setSocketOption skt ReuseAddr 1
    withFdSocket skt setCloseOnExecIfNeeded
    bind skt $ addrAddress addr
    listen skt 1
    (sock, raddr) <- accept skt
    putStrLn $ "Get player from" ++ show raddr
    return sock

waitForPlacement :: Socket -> BChan GobangEvent  -> IO ()
waitForPlacement sock chan = do
    msg <- recv sock 1024
    let p@(Position c r) = decode $ fromStrict msg :: Payload
    print p
    writeBChan chan $ Placement (c, r)
    return ()

sendPlacement :: Int -> Int -> Socket -> IO ()
sendPlacement x y sock = do
    let pl = Position x y
    _ <- send sock $ toStrict $ encode pl
    return ()

serverMain :: BChan GobangEvent -> IO ()
serverMain chan = do
    sock <- createRoom
    _ <- forever $ waitForPlacement sock chan
    return ()

clientMain :: IO ()
clientMain = do
    sock <- joinGame
    _ <- forever $ getAndSend sock
    return ()
    where
        getAndSend sock = do
            putStrLn "Send Position: x y"
            l <- getLine
            let xy = map read (words l) :: [Int]
            sendPlacement (head xy) (last xy) sock
            return ()