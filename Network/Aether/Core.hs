module Network.Aether.Core
    (
    ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString as B
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString

maxRecv :: Int
maxRecv = 4096

hints :: AddrInfo
hints = defaultHints {addrFlags = [AI_PASSIVE]}

serve :: PortNumber -> ((B.ByteString, SockAddr) -> IO ()) -> IO ()
serve port handler = do
    (AddrInfo _ family _ _ addr _):_ <- getAddrInfo (Just hints) Nothing (Just $ show port)
    sock <- socket family Datagram defaultProtocol
    bind sock addr
    -- forever $ recvFrom sock maxRecv >>= (forkIO . handler)
    finally (forever $ recvFrom sock maxRecv >>= (forkIO . handler)) (close sock)
