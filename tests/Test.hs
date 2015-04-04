module Test where

import           Network.Aether.Types
import           Network.Aether.Flow

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.Word
import           Network.Socket hiding (sendTo)
import           Network.Socket.ByteString

a = Addr (183949123 :: Word32) 8991

test = do
    r <- ask Env False a (Ping 43829051)
    print r
