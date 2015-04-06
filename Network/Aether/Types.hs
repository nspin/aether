{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Aether.Types
    (
      Env(..)
  --
    , Method (Response, decodeQuery, decodeResp)
    , pkgQuery
    , pkgResp
    , pkgError

    , Ping(..)
    , FindNode(..)
    , GetPeers(..)
    , AnnouncePeer(..)
    , GetVal(..)
    , GetVar(..)
  --
    , Error
    , ErrorCode
    , decodeError
  --
    , Want
    , parseWant
    , buildWant

    , IP(..)
    , IPv4
    , IPv6

    , Addr(..)
    , parseAddr
    , buildAddr

    , Node(..)
    , parseNode
    , buildNode

    , NodeList(..)
    , decodeNodeList
    , encodeNodeList

    ) where

import           Network.Aether.Prelude

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Attoparsec.ByteString hiding (word8)
import           Data.Aencode
import           Data.BigWord
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

data Env (n :: Nat) = Env

------------------
-- METHOD
------------------

class Method a where
    name :: a -> B.ByteString
    data Response a :: *
    writeQuery :: a -> Mappy
    writeResp  :: Response a -> Mappy
    decodeQuery :: BDict B.ByteString -> Maybe a
    decodeResp  :: BDict B.ByteString -> Maybe (Response a)
    -- respond    :: Env -> a -> STM (Either Error (Response a))

pkgQuery :: Method a => Bool -> a -> B.ByteString -> BDict IBuilder
pkgQuery ro q tid = M.fromList $
    [ ("t", pbs tid)
    , ("y", pbs "q")
    , ("q", pbs $ name q)
    , ("a", BDict $ M.fromList $ writeQuery q)
    ] ++ ( if ro
           then [("ro", BInt 1)]
           else []
         )

pkgResp :: Method a => Response a -> B.ByteString -> BDict IBuilder
pkgResp r tid = M.fromList
    [ ("t", pbs tid)
    , ("y", pbs "r")
    , ("a", BDict $ M.fromList $ writeResp r)
    ]

pkgError :: Error -> B.ByteString -> BDict IBuilder
pkgError (Error c s) tid = M.fromList
    [ ("t", pbs tid)
    , ("y", pbs "e")
    , ("a", BList   $ [BInt (codeOf c), pbs s])
    ]

------------------
-- METHODS
------------------

data Ping = Ping (W 160) deriving Show

instance Method Ping where
    name _ = "ping"
    data Response Ping = Pong (W 160) deriving Show
    writeQuery (Ping id) = [("id", BString $ prefixBE id)]
    writeResp  (Pong id) = [("id", BString $ prefixBE id)]
    decodeQuery = fmap Ping . (M.lookup "id" >=> getBString >=> onlyDo parseBE)
    decodeResp  = fmap Pong . (M.lookup "id" >=> getBString >=> onlyDo parseBE)

data FindNode = FindNode { id_f :: W 160
                         , target :: W 160
                         , want :: [Want]
                         }

instance Method FindNode where
    data Response FindNode = FoundNode { id_fr :: W 160
                                       , nodes_f :: NodeList
                                       }
    writeQuery (FindNode i t w) = [ ("id", BString $ prefixBE i)
                                  , ("target", BString $ prefixBE t)
                                  , ("want", BList $ map (BString . buildWant) w)
                                  ]
    writeResp (FoundNode i ns) = ("id", BString $ prefixBE i) : encodeNodeList ns

data GetPeers = GetPeers { id_g :: W 160
                         , info_hash_g :: W 160
                         , noseed :: Bool
                         , scrape :: Bool
                         }

instance Method GetPeers where
    data Response GetPeers = GotPeers { id_gr :: W 160
                                      , nodes_g :: NodeList
                                      , values :: Maybe [Either (Addr IPv4) (Addr IPv6)]
                                      }

data AnnouncePeer = AnnouncePeer { id_a :: W 160
                                 , info_hash_a :: W 160
                                 , port_a :: Maybe (W 16)
                                 , token :: B.ByteString
                                 , seed :: Bool
                                 }

data GetVal = GetVal

data GetVar = GetVar

-------------------
-- ERROR
-------------------

data Error = Error ErrorCode B.ByteString deriving Show

data ErrorCode = Generic | Server | Protocol | Method | Other Integer deriving Show

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

data Want = N4 | N6

parseWant :: Parser Want
parseWant = (N4 <$ string "n4") <|> (N6 <$ string "n6")

buildWant :: Want -> IBuilder
buildWant N4 = prefix ("n4" :: B.ByteString)
buildWant N6 = prefix ("n6" :: B.ByteString)

class KnownNat n => IP n where
    sayAddr :: Addr n -> SockAddr
    family :: proxy n -> Family

type IPv4 = 32
type IPv6 = 128

instance IP IPv4 where
    sayAddr (Addr ip port) = SockAddrInet (PortNum $ fromIntegral port) (fromIntegral ip)
    family _ = AF_INET

instance IP IPv6 where
    sayAddr (Addr ip port) = SockAddrInet6 (PortNum $ fromIntegral port) 0 (toTuple ip) 0
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

data Addr n = Addr
    { ip   :: W n
    , port :: W 16
    }

parseAddr :: IP a => Parser (Addr a)
parseAddr = Addr <$> parseBE <*> parseBE

buildAddr :: IP a => Addr a -> IBuilder
buildAddr (Addr i p) = prefixBE i <> prefixBE p

data Node a = Node
    { nid  :: W 160
    , addr :: Addr a
    }

parseNode :: IP a => Parser (Node a)
parseNode = Node <$> parseBE <*> parseAddr

buildNode :: IP a => Node a -> IBuilder
buildNode (Node n a) = prefixBE n <> buildAddr a

data NodeList = NodeList { nodes  :: Maybe [Node IPv4]
                         , nodes6 :: Maybe [Node IPv6]
                         }

decodeNodeList :: BDict B.ByteString -> NodeList
decodeNodeList m = NodeList (go "nodes") (go "nodes6")
  where
    go :: IP a => B.ByteString -> Maybe [Node a]
    go = (`M.lookup` m) >=> getBString >=> onlyDo (many' parseNode)

encodeNodeList :: NodeList -> Mappy
encodeNodeList (NodeList n4 n6) = catMaybes [go "nodes" n4, go "nodes6" n6]
  where go str = fmap $ (,) str . BString . mcatmap buildNode

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

------------------
-- HELPERS
------------------

type Mappy = [(B.ByteString, BValue IBuilder)]

