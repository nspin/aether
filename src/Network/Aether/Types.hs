{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Aether.Types
    (
      Env(..)

    , Query(..)
    , Request(..)

    , Method(..)

    , Ping(..)
    , FindNode(..)
    , GetPeers(..)
    , AnnouncePeer(..)
    , GetVal(..)
    , GetVar(..)

    , Error(..)
    , ErrorCode(..)

    , Want(..)

    , IP(..)

    , AnAddr(..)

    , Addr(..)

    , Node(..)
    , NodeList(..)

    ) where

import           Network.Aether.Prelude

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.ByteString hiding (word8)
import           Data.Bencode
import           Data.Word.N
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Maybe
import qualified Data.Map as M
import           Data.Time.Clock
import           Data.Word
import           GHC.TypeLits
import           Network.Socket

data Env = Env

data Request n a = Request
    { readOnly :: Bool
    , sender   :: Addr n
    , request  :: a
    }

data Query w = APing         (w Ping)
             | AFindNode     (w FindNode)
             | AGetPeers     (w GetPeers)
             | AAnnouncePeer (w AnnouncePeer)
             | AGetVal       (w GetVal)
             | AGetVar       (w GetVar)

------------------
-- METHOD
------------------

class Method a where
    data Response a :: *
    name        :: a -> B.ByteString
    writeQuery  :: a -> Mappy
    writeResp   :: Response a -> Mappy
    decodeQuery :: BDict B.ByteString -> Maybe a
    decodeResp  :: BDict B.ByteString -> Maybe (Response a)
    respond     :: IP n => Env -> Query n -> IO (Either Error (Response a))

------------------
-- METHODS
------------------

data Ping = Ping
    { p_id :: W 160
    } deriving Show

data FindNode = FindNode
    { f_id     :: W 160
    , f_target :: W 160
    , f_want   :: [Want]
    } deriving Show

data GetPeers = GetPeers
    { g_id        :: W 160
    , g_info_hash :: W 160
    , g_noseed    :: Bool
    , g_scrape    :: Bool
    } deriving Show

data AnnouncePeer = AnnouncePeer
    { a_id        :: W 160
    , a_info_hash :: W 160
    , a_port      :: Maybe (W 16)
    , a_token     :: B.ByteString
    , a_seed      :: Bool
    } deriving Show

data GetVal = GetVal

data GetVar = GetVar

-------------------
-- ERROR
-------------------

data Error = Error
    { e_code :: ErrorCode
    , e_msg  :: B.ByteString
    }

data ErrorCode = Generic
               | Server
               | Protocol
               | Method
               | Other Integer

-------------------
-- OTHER
-------------------

data Want = N4 | N6

class KnownNat n => IP n where
    sayAddr  :: Addr n -> SockAddr
    readAddr :: SockAddr -> Maybe (Addr n)
    family   :: proxy n -> Family

data AnAddr = IPv4 (Addr 32) | IPv6 (Addr 128)

data Addr n = Addr
    { addrIp   :: W n
    , addrPort :: W 16
    }

data Node a = Node
    { nodeId   :: W 160
    , nodeAddr :: Addr a
    }

data NodeList = NodeList
    { nodes  :: Maybe [Node IPv4]
    , nodes6 :: Maybe [Node IPv6]
    }

------------------
-- WAT
------------------

-- data Peer a = Peer
--     { node :: Node a
--     , last :: UTCTime
--     , answered :: Bool
--     }

-- type RespState = M.Map B.ByteString TMVar

-- -- Acidic
type Buckets a = [[Node a]]
