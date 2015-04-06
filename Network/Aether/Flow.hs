{-# LANGUAGE OverloadedStrings #-}

module Network.Aether.Flow
    ( ask
    ) where

import           Network.Aether.Types

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Network.Socket hiding (sendTo)
import           Network.Socket.ByteString

post :: Env a -> STM (B.ByteString, TMVar (Maybe (Either Error (BDict B.ByteString))))
post _ = return ("hi", undefined)

emme :: Either a (Maybe b) -> Maybe (Either a b)
emme (Left  l) = Just $ Left l
emme (Right r) = Right <$> r

ask :: (IP n, Method b) => Env n -> Bool -> Addr n -> b -> IO (Maybe (Either Error (Response b)))
ask env ro addr q = do
    (tid, hole) <- atomically $ post env
    udpass addr . L.toStrict . toLazyByteString . buildBDict $ pkgQuery ro q tid
    r <- atomically $ takeTMVar hole
    return $ do
        e <- r
        emme $ fmap decodeResp e

udpass :: IP n => Addr n -> B.ByteString -> IO Int
udpass addr pkt = bracket (socket (family $ ip addr) Datagram defaultProtocol)
                          sClose
                          (\s -> sendTo s pkt (sayAddr addr))
