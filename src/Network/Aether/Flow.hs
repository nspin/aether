{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Network.Aether.Flow
    ( ask
    ) where

import           Network.Aether.Types
import           Network.Aether.Prelude
import           Network.Aether.RW

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           GHC.TypeLits
import           Network.Socket hiding (sendTo)
import           Network.Socket.ByteString

serve :: PortNumber -> ((B.ByteString, SockAddr) -> IO ()) -> IO ()
serve port handler = do
    (AddrInfo _ family _ _ addr _):_ <- getAddrInfo (Just hints) Nothing (Just $ show port)
    sock <- socket family Datagram defaultProtocol
    bind sock addr
    -- forever $ recvFrom sock maxRecv >>= (forkIO . handler)
    finally (forever $ recvFrom sock maxRecv >>= (forkIO . handler)) (close sock)

handler :: Mailroom -> (B.ByteString, SockAddr) -> IO ()
handler (Mailroom i o t) = 

data Mailroom = Mailroom
    { inBound  :: M.Map B.ByteString (TMVar (      B.ByteString))
    , outBound :: M.Map B.ByteString (TMVar (BDict B.ByteString))
    , tids     :: [W 8]
    }

post :: Mailroom -> STM (B.ByteString, TMVar (Maybe (Either Error (BDict B.ByteString))))
post (Mailroom _ out) = return ("hi", undefined)

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
