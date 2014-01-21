{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent         (forkIO)
import           Control.Exception          (bracket, finally)
import           Control.Monad              (forever, when)
import qualified Data.ByteString.Lazy            as B
import qualified Data.ByteString.Lazy.Char8 as C
import           HttpRequestParser          (HttpRequestHeadLine (..),
                                             parseRequest)
import           Network                    (PortID (..), listenOn)
import           Network.Socket
import           Network.Socket.ByteString  (sendAll)
import           System.IO


main :: IO ()
main = withSocketsDo $ bracket (listenOn $ PortNumber 9000) sClose loop
    where
--      loop s = forever $ forkIO . request . fst =<< accept s
        loop s = forever $ forkIO . handleRequest . fst =<< accept s
--      request c = sendAll c resp `finally` sClose c


resp :: String
resp = "HTTP/1.0 200 OK\nContent-Length: 16\n\nGoodbye, World!\n"

handleRequest :: Socket -> IO ()
handleRequest = readAndParseRequest
    where
        readAndParseRequest = readRequest

        -- read request
        readRequest sock = do
            h <- socketToHandle sock ReadWriteMode
            msgs <- B.hGetContents h
            req <- handleMessage msgs
            print . show $ req
            hPutStr h resp
            hClose h

        handleMessage :: B.ByteString -> IO HttpRequestHeadLine
        handleMessage = return . parseRequest . C.unpack

