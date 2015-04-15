{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Aether.Prelude
    ( pbs
    , parseBE
    , prefixBE
    , mcatmap
    , onlyDo
    , IBuilder
    , Word8
    , Word16
    , Word32
    , Word64
    , Word128
    , Word96
    , Word160
    ) where

import           Control.Lens
import           Data.Aencode
import           Data.Attoparsec.ByteString hiding (word8)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.LargeWord.Lens
import           Data.Monoid
import qualified Data.Map as M
import           Data.Word (Word8)

pbs :: B.ByteString -> BValue IBuilder
pbs = BString . prefix

pbe :: forall a. (Homogenous a Word8) => BValue IBuilder
pbe = BString . (prefixBE :: a -> IBuilder)

parseBE :: (Homogenous a Word8) => Parser a
parseBE = leaves (const anyWord8) (populate (undefined :: Word8))

prefixBE :: forall a. Homogenous a Word8 => a -> IBuilder
prefixBE w = (Sum $ fromIntegral $ lengthOf (leaves :: Traversal' a Word8)  w, foldMapOf (leaves :: Traversal' a Word8) word8 w)

mcatmap :: Monoid m => (a -> m) -> [a] -> m
mcatmap = (.) mconcat . map

onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo p = maybeResult . (`feed` B.empty) . parse (p <* endOfInput)

type IBuilder = (Sum Integer, Builder)

type Word16  = LargeKey Word8  Word8
type Word32  = LargeKey Word16 Word16
type Word64  = LargeKey Word32 Word32
type Word128 = LargeKey Word64 Word64

type Word96  = LargeKey Word64  Word32
type Word160 = LargeKey Word128 Word32
