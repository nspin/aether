{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Aether.RW
    (

      pkgMsg
    , pkgQuery
    , pkgResp
    , pkgError

    , decodeError

    ) where

import           Network.Aether.Prelude
import           Network.Aether.Types

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

------------------
-- METHOD
------------------

pkgMsg :: B.ByteString -> Mappy -> BDict B.ByteString
pkgMsg tid = M.fromList . (:) ("t", BString tid)

pkgQuery :: Method a => Bool -> a -> Mappy
pkgQuery ro q =
    [ ("y", BString "q")
    , ("q", BString $ name q)
    , ("a", BDict $ M.fromList $ writeQuery q)
    ] ++ ( if ro
           then [("ro", BInt 1)]
           else []
         )

pkgResp :: Method a => Response a -> Mappy
pkgResp r =
    [ ("y", BString "r")
    , ("a", BDict $ M.fromList $ writeResp r)
    ]

pkgError :: Error -> Mappy
pkgError (Error c s) =
    [ ("y", BString "e")
    , ("a", BList [BInt (codeOf c), BString s])
    ]

------------------
-- METHODS
------------------

instance Method Ping where

    data Response Ping = Pong
        { pr_id :: (W 160)
        }

    name _ = "ping"

    writeQuery (Ping id) = [("id", BString $ prefixBE id)]

    writeResp  (Pong id) = [("id", BString $ prefixBE id)]

    decodeQuery = fmap Ping . (M.lookup "id" >=> getBString >=> onlyDo parseBE)

    decodeResp  = fmap Pong . (M.lookup "id" >=> getBString >=> onlyDo parseBE)

instance Method FindNode where

    data Response FindNode = FoundNode
        { fr_id :: W 160
        , fr_nodes :: NodeList
        }

    writeQuery (FindNode i t w) =
        [ ("id", BString $ prefixBE i)
        , ("target", BString $ prefixBE t)
        , ("want", BList $ map (BString . buildWant) w)
        ]

    writeResp (FoundNode i ns) = ("id", BString $ prefixBE i) : encodeNodeList ns

instance Method GetPeers where

    data Response GetPeers = GotPeers
        { gr_id     :: W 160
        , gr_nodes  :: NodeList
        , gr_values :: Maybe [Either (Addr IPv4) (Addr IPv6)]
        }

instance Method GetVal where

instance Method GetVar where

-------------------
-- ERROR
-------------------

decodeError :: [BValue B.ByteString] -> Maybe Error
decodeError (BInt n : BString str : _) = Just $ Error (nameOf n) str
decodeError _ = Nothing

nameOf :: Integer -> ErrorCode
nameOf 201 = Generic
nameOf 202 = Server
nameOf 203 = Protocol
nameOf 204 = Method
nameOf n   = Other n

codeOf :: ErrorCode -> Integer
codeOf Generic   = 201
codeOf Server    = 202
codeOf Protocol  = 203
codeOf Method    = 204
codeOf (Other n) = n

-------------------
-- OTHER
-------------------

parseWant :: Parser Want
parseWant = (N4 <$ string "n4") <|> (N6 <$ string "n6")

buildWant :: Want -> B.ByteString
buildWant N4 = "n4"
buildWant N6 = "n6"

instance IP 32 where
    sayAddr (Addr ip port) = SockAddrInet (PortNum $ fromIntegral port) (fromIntegral ip)
    readAddr (SockAddr (PortNum p) i) = Addr (fromIntegral i) (fromIntegral p)
    family _ = AF_INET

instance IP 128 where
    sayAddr (Addr ip port) = SockAddrInet6 (PortNum $ fromIntegral port) 0 (toTuple ip) 0
    readAddr (SockAddr (PortNum p) _ i _) = Addr (fromTuple i) (fromIntegral p)
    family _ = AF_INET6

toTuple :: W 128 -> HostAddress6
toTuple w = over each (fromIntegral :: W 32 -> Word32) (a, b, c, d)
  where
    (a, x) = split w
    (b, y) = split x
    (c, d) = split y

fromTuple :: HostAddress6 -> W 128
fromTuple x = case over each (fromIntegral :: Word32 -> W 32) x of
    (a, b, c, d) -> a >+< b >+< c >+< d

parseAddr :: IP a => Parser (Addr a)
parseAddr = Addr <$> parseBE <*> parseBE

buildAddr :: IP a => Addr a -> B.ByteString
buildAddr (Addr i p) = prefixBE i <> prefixBE p

parseNode :: IP a => Parser (Node a)
parseNode = Node <$> parseBE <*> parseAddr

buildNode :: IP a => Node a -> B.ByteString
buildNode (Node n a) = prefixBE n <> buildAddr a

decodeNodeList :: BDict B.ByteString -> NodeList
decodeNodeList m = NodeList (go "nodes") (go "nodes6")
  where
    go :: IP a => B.ByteString -> Maybe [Node a]
    go = (`M.lookup` m) >=> getBString >=> onlyDo (many' parseNode)

encodeNodeList :: NodeList -> Mappy
encodeNodeList (NodeList n4 n6) = catMaybes [go "nodes" n4, go "nodes6" n6]
  where go str = fmap $ (,) str . BString . mcatmap buildNode

