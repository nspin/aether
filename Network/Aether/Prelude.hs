{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Aether.Prelude
    ( parseBE
    , prefixBE
    , mcatmap
    , onlyDo
    ) where

import           Control.Lens
import           Data.Aencode
import           Data.Attoparsec.ByteString hiding (word8)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.BigWord
import           Data.Monoid
import qualified Data.Map as M
import           Data.Word (Word8)

pbs :: Stringable s => s -> BValue IBuilder
pbs = String . prefix

parseBE :: KnownNat n => Parser (W n)
parseBE = takeBE ((fromIntegral :: Word8 -> W 8) . anyWord8)

prefixBE :: forall n. KnownNat n => W n -> IBuilder
prefixBE w = (Sum bytes, giveBE (word8 . (fromIntegral :: W 8 -> Word8)))
  where bytes = let (q, r) = natVal (Proxy :: Proxy n) `quotRem` 8
                in case r of 0 -> q
                             _ -> q + 1

mcatmap :: Monoid m => (a -> m) -> [a] -> m
mcatmap = (.) mconcat . map

onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo p = maybeResult . (`feed` B.empty) . parse (p <* endOfInput)
