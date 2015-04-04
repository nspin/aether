{-# LANGUAGE TypeFamilies #-}

module Network.Aether.Types
    (
    ) where

import           Network.Aether.Internal

import           Control.Concurrent.STM
import           Data.Acid
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Time.Clock
import           Data.Word

data Env = Env

class Method a where
    data Response a :: *
    parseQuery :: BDict B.ByteString -> Maybe a
    parseResp  :: BDict B.ByteString -> Maybe a
    buildQuery :: Env -> Response a -> BDict Builder
    buildResp  :: Env -> Response a -> BDict Builder
    respond    :: Env -> a -> STM (Either Error (Response a))

build

data Error = Error ErrorCode B.ByteString

data ErrorCode = Generic | Server | Protocol | Method | Other Integer

data Ping = Ping

data FindNode = FindNode Word160

data GetPeers = GetPeers { info_hash_g :: Word160
                         , noseed :: Bool
                         , scrape :: Bool
                         }

data AnnouncePeer = AnnouncePeer { info_hash_a :: Word160
                                 , port_a :: Maybe Word160
                                 , token :: B.ByteString
                                 , seed :: Bool
                                 }

data GetVal = GetVal 

data GetVar = GetVar 

data Node a = Node
    { location :: Word160
    , port_n   :: Word16
    , addr     :: a
    }

data Peer a = Peer
    { node :: Node a
    , last :: UTCTime
    , answered :: Bool
    }

-- type RespState = M.Map B.ByteString TMVar

-- -- Acidic
type Buckets a = [[Peer a]]

