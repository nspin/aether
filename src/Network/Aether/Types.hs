module Network.Aether.Types
    (
    ) where

import           Data.Acid
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.Concurrent.STM
import           Data.Word.Play

data Message = Message
    { transaction :: B.ByteString
    , readOnly :: Bool
    , content :: Content
    }

data Content = Query
                { id :: Word160
                , query :: Query
                }
             | Response
             | Error Integer B.ByteString

data Query = Ping
                { id :: Word160
                }
           | FindNode
                { target :: Word160
                }
           | GetPeers
                { infoHash :: Word160
                , noseed :: Bool
                , scrape :: Bool
                }
           | AnnouncePeer
                { infoHash :: Word160
                , port :: Maybe Word16
                , token :: B.ByteString
                , seed :: Bool
                }
           | GetVal
           | GetVar

data Response = Pong Word160
              | FoundNode [Node]
              | GotPeers
                    { token :: B.ByteString
                    , values :: [Peer]
                    }

instance ToAeson Message where
    encode = undefined

instance FromAeson Message where
    decode = undefined

data Node a = Node
    { location :: Word160
    , port     :: Word16
    , addr     :: a
    }

data Peer a = Peer
    { node :: Node
    , last :: UnixTime
    , answered :: Bool
    }

type RespState = M.Map B.ByteString TMVar

-- Acidic
type Buckets = [[Node]]

