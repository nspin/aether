module Network.Types
    (
    ) where

import           Data.Concurrent.STM
import           Data.Aencode
import           Data.Acid
import qualified Data.ByteString as B
import           Data.Word.Play

data Message = undefined

instance ToAeson Message where

instance FromAeson Message where

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

