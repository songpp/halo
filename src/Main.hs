{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main where

import           Control.Concurrent         (forkIO)
import           Control.Exception          (bracket, finally)
import           Control.Monad              (forever, when)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           HttpRequestParser          (Header (..), HttpRequest (..), HttpRequestLine (..),
                                             parseRequest)
import           Network                    (PortID (..), listenOn)
import           Network.Socket
import           Network.Socket.ByteString  (sendAll)
import           System.IO


main :: IO ()
main = withSocketsDo $ bracket (listenOn $ PortNumber 9000) sClose loop
    where
        loop s = forever $ forkIO . handleConnection . fst =<< accept s


resp :: String
resp = "HTTP/1.1 200 OK\nContent-Length: 16\n\nGoodbye, World!\n"

handleConnection :: Socket -> IO ()
handleConnection sock = bracket (socketToHandle sock ReadWriteMode) hClose process
    where
        process :: Handle -> IO ()
        process h = do
            msgs <- B.hGetContents h
            req <- parse msgs
            handleRequest (req, h)

        parse :: B.ByteString -> IO HttpRequest
        parse = return . parseRequest . C.unpack


handleRequest :: (HttpRequest, Handle) -> IO ()
handleRequest (req, conn) = do
    print . show $ req
    hPutStr conn resp


