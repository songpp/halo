{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module Monads where


import           "mtl" Control.Monad.Cont
import           "mtl" Control.Monad.Identity
import           Data.Bits              (xor)
import qualified Data.ByteString        as B
import           Data.ByteString.Base64 (decode, encode)


class M m where
    ret :: a -> m a
    (>>>=) :: m a -> (a -> m b) -> m b


instance M ((->) r) where
    ret x = \_ -> x
    h >>>= f = \w -> f (h w) w

infixl 9 >>>=



key = "this is key"

encrypt = B.pack . B.zipWith xor key
decrypt = encrypt

