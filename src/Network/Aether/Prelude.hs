{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Aether.Prelude
    ( parseBE
    , prefixBE
    , mcatmap
    , onlyDo
    , Mappy
    , maxRecv
    , hints
    ) where

import           Control.Lens
import           Data.Bencode
import           Data.Attoparsec.ByteString hiding (word8)
import qualified Data.ByteString as B
import           Data.Word.N
import           Data.Monoid
import qualified Data.Map as M
import           Data.Proxy
import           Data.Word (Word8)
import           GHC.TypeLits
import           Network.Socket hiding (sendTo)
import           Network.Socket.ByteString

parseBE :: KnownNat n => Parser (W n)
parseBE = accumulate' (fmap (fromIntegral :: Word8 -> W 8) anyWord8)

prefixBE :: forall n. KnownNat n => W n -> B.ByteString
prefixBE w = chunks' (B.singleton . (fromIntegral :: W 8 -> Word8)) w
  where bytes = let (q, r) = natVal (Proxy :: Proxy n) `quotRem` 8
                in case r of 0 -> q
                             _ -> q + 1

mcatmap :: Monoid m => (a -> m) -> [a] -> m
mcatmap = (.) mconcat . map

onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo p = maybeResult . (`feed` B.empty) . parse (p <* endOfInput)

type Mappy = [(B.ByteString, BValue B.ByteString)]

maxRecv :: Int
maxRecv = 4096

hints :: AddrInfo
hints = defaultHints {addrFlags = [AI_PASSIVE]}
