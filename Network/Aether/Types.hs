module Network.Aether.Types
    (
    ) where

import           Data.Acid
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.Concurrent.STM

-- data Message = Message
--     { transaction :: B.ByteString
--     , content :: Content
--     }

-- data Content = Query Bool Word160 Query
--              | Response Word160 Response
--              | Error Error String

-- data Query = Pong
--            | FindNode
--                 { target :: Word160
--                 }
--            | GetPeers
--                 { infoHash :: Word160
--                 , noseed :: Bool
--                 , scrape :: Bool
--                 }
--            | AnnouncePeer
--                 { infoHash :: Word160
--                 , port :: Maybe Word16
--                 , token :: B.ByteString
--                 , seed :: Bool
--                 }
--            | GetVal
--            | GetVar

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

