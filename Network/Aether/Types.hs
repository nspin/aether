{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedString #-}

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

------------------
-- METHODS
------------------

class Method a where
    data Response a :: *
    encodeQuery :: a -> BDict Builder
    encodeResp  :: Response a -> BDict Builder
    decodeQuery :: BDict B.ByteString -> Maybe a
    decodeResp  :: BDict B.ByteString -> Maybe (Response a)
    -- respond    :: Env -> a -> STM (Either Error (Response a))

data Ping = Ping Word160

instance Method Ping where
    data Response Ping = Pong Word160
    encodeQuery (Ping id) = M.singleton (string8 "id", id)
    encodeResp  (Pong id) = M.singleton (string8 "id", id)
    decodeQuery = fmap Ping . M.lookup "id"
    decodeResp  = fmap Pong . M.lookup "id"

data FindNode = FindNode { id_f :: Word160
                         , target :: Word160
                         }

data GetPeers = GetPeers { id_g :: Word160
                         , info_hash_g :: Word160
                         , noseed :: Bool
                         , scrape :: Bool
                         }

data AnnouncePeer = AnnouncePeer { id_a :: Word160
                                 , info_hash_a :: Word160
                                 , port_a :: Maybe Word160
                                 , token :: B.ByteString
                                 , seed :: Bool
                                 }

data GetVal = GetVal

data GetVar = GetVar

-------------------
-- ERROR
-------------------

data Error = Error ErrorCode B.ByteString

data ErrorCode = Generic | Server | Protocol | Method | Other Integer

codeOf :: ErrorCode -> Integer
codeOf Generic   = 201
codeOf Server    = 202
codeOf Protocol  = 203
codeOf Method    = 204
codeOf (Other n) = n

encodeError :: Error -> [BValue B.ByteString]
encodeError (Error c s) = [BInt (codeOf c), BString s]

-------------------
-- OTHER
-------------------

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

