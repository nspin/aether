module Network.Aether.Internal
    ( Word128
    , Word160
    , fromTuple
    ) where

import           Data.LargeWord (LargeKey(..))
import           Data.Word
import           Network.Socket


type Word96  = LargeKey Word32 (LargeKey Word32 Word32)
type Word128 = LargeKey Word32 Word96
type Word160 = LargeKey Word32 Word128

fromTuple :: HostAddress6 -> Word128
fromTuple (a, b, c, d) = LargeKey a . LargeKey b $ LargeKey c d
