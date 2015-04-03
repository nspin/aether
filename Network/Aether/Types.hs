{-# LANGUAGE TypeFamilies #-}

module Network.Aether.Types
    (
    ) where

import           Network.Aether.Internal

import           Control.Concurrent.STM
import           Data.Acid
import           Data.Aencode
import qualified Data.ByteString as B

type BBDict = BDict B.ByteString

-- data Query 

-- class Method a where
--     data Response a :: *

-- data Message = Message
--     { transaction :: B.ByteString
--     , content :: Content
--     }

-- data Content = Query Bool Word160 Query
--              | Response Word160 Response
--              | Error Error String

data Method = Ping
                 { id :: Word160
                 }
            | FindNode
                 { id :: Word160
                 , target :: Word160
                 }
            | GetPeers
                 { id :: Word160
                 , info_hash :: Word160
                 , noseed :: Bool
                 , scrape :: Bool
                 }
            | AnnouncePeer
                 { id :: Word160
                 , info_hash :: Word160
                 , port :: Maybe Word160
                 , token :: B.ByteString
                 , seed :: Bool
                 }
            | GetVal
            | GetVar

-- data Response = Pong Word160
--               | FoundNode
--                     { nodes :: [Node Word32]
--                     , nodes6 :: [Node Word128]
--                     }
--               | GotPeers
--                     { token :: B.ByteString
--                     , answer :: Either [Peer] [Node a]
--                     }

-- data Error = Generic | Server | Protocol | Method | Other Integer

-- instance ToAeson Message where
--     encode = undefined

-- instance FromAeson Message where
--     decode = undefined

-- data Node a = Node
--     { location :: Word160
--     , port     :: Word16
--     , addr     :: a
--     }

-- data Peer a = Peer
--     { node :: Node
--     , last :: UnixTime
--     , answered :: Bool
--     }

-- type RespState = M.Map B.ByteString TMVar

-- -- Acidic
-- type Buckets = [[Node]]

