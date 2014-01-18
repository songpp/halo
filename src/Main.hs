{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent        (forkIO)
import           Control.Exception         (bracket, finally)
import           Control.Monad             (forever)
import           Network                   hiding (accept)
import           Network.Socket            (accept)
import           Network.Socket.ByteString (sendAll)


main :: IO ()
main = bracket (listenOn $ PortNumber 9000) sClose loop 
    where
        loop s = forever $ forkIO . request . fst =<< accept s
        request c = sendAll c resp `finally` sClose c
        resp = "HTTP/1.0 200 OK\nContent-Length: 16\n\nGoodbye, World!\n"
