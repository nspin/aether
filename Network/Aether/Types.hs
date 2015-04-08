{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Aether.Types
    (
    ) where

import           Network.Aether.Internal

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Data.Acid
import           Data.Attoparsec.ByteString
import           Data.Aencode
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import           Data.Monoid
import           Data.Maybe
import qualified Data.Map as M
import           Data.Time.Clock
import           Data.Word
import           Data.Wordplay
import           Network.Socket

data Env = Env

------------------
-- METHODS
------------------

type Mappy = [(B.ByteString, BValue B.ByteString Builder)]

class Method a where
    data Response a :: *
    writeQuery :: a -> Mappy
    writeResp  :: Response a -> Mappy
    decodeQuery :: BDict B.ByteString -> Maybe a
    decodeResp  :: BDict B.ByteString -> Maybe (Response a)
    -- respond    :: Env -> a -> STM (Either Error (Response a))

encode :: Mappy -> BDict Builder
encode = M.fromList . over (traverse._1) byteString

data Ping = Ping Word160

instance Method Ping where
    data Response Ping = Pong Word160
    writeQuery (Ping id) = [("id", BString $ buildBE id)]
    writeResp  (Pong id) = [("id", BString $ buildBE id)]
    decodeQuery = fmap Ping . (M.lookup "id" >=> asString >=> onlyDo parseBE)
    decodeResp  = fmap Pong . (M.lookup "id" >=> asString >=> onlyDo parseBE)

data FindNode = FindNode { id_f :: Word160
                         , target :: Word160
                         , want :: [Want]
                         }

instance Method FindNode where
    data Response FindNode = FoundNode { id_fr :: Word160
                                       , nodes_f :: NodeList
                                       }
    writeQuery (FindNode i t w) = [ ("id", BString $ buildBE i)
                                  , ("target", BString $ buildBE t)
                                  , ("want", BList $ map (BString . buildWant) w)
                                  ]
    writeResp (FoundNode i ns) = ("id", BString $ buildBE i) : encodeNodeList ns

data GetPeers = GetPeers { id_g :: Word160
                         , info_hash_g :: Word160
                         , noseed :: Bool
                         , scrape :: Bool
                         }

instance Method GetPeers where
    data Response GetPeers = GotPeers { id_gr :: Word160
                                      , nodes_g :: NodeList
                                      , values :: Maybe [Either (Addr IPv4) (Addr IPv6)]
                                      }

data AnnouncePeer = AnnouncePeer { id_a :: Word160
                                 , info_hash_a :: Word160
                                 , port_a :: Maybe Word160
                                 , token :: B.ByteString
                                 , seed :: Bool
                                 }

data GetVal = GetVal

data GetVar = GetVar

-------------------
-- ERROR
-------------------

data Error = Error ErrorCode B.ByteString

data ErrorCode = Generic | Server | Protocol | Method | Other Integer

nameOf :: Integer -> ErrorCode
nameOf 201 = Generic
nameOf 202 = Server
nameOf 203 = Protocol
nameOf 204 = Method
nameOf n   = Other n

decodeError :: [BValue B.ByteString] -> Maybe Error
decodeError (BInt n : BString str : _) = Just $ Error (nameOf n) str
decodeError _ = Nothing

codeOf :: ErrorCode -> Integer
codeOf Generic   = 201
codeOf Server    = 202
codeOf Protocol  = 203
codeOf Method    = 204
codeOf (Other n) = n

encodeError :: Error -> [BValue B.ByteString]
encodeError (Error c s) = [BInt (codeOf c), BString s]

-------------------
-- OTHER
-------------------

data Want = N4 | N6

parseWant :: Parser Want
parseWant = (N4 <$ string "n4") <|> (N6 <$ string "n6")

buildWant :: Want -> Builder
buildWant N4 = byteString "n4"
buildWant N6 = byteString "n6"

class IP a where
    meetIP :: Addr a -> ((SockAddr, Socket) -> IO a) -> IO a
    seeIP :: SockAddr -> Maybe a
    parseIP :: Parser a
    writeIP :: a -> Builder

type IPv4 = Word32
type IPv6 = Word128

instance IP IPv4 where
    meetIP = undefined
    seeIP = undefined
    parseIP = parseBE
    writeIP = buildBE

instance IP IPv6 where
    meetIP = undefined
    seeIP = undefined
    parseIP = parseBE
    writeIP = buildBE

data Addr a = Addr
    { ip   :: a
    , port :: Word16
    }

parseAddr :: IP a => Parser (Addr a)
parseAddr = Addr <$> parseIP <*> parseBE

buildAddr :: IP a => Addr a -> Builder
buildAddr (Addr i p) = writeIP i <> buildBE p

data Node a = Node
    { nid  :: Word160
    , addr :: Addr a
    }

parseNode :: IP a => Parser (Node a)
parseNode = Node <$> parseBE <*> parseAddr

buildNode :: IP a => Node a -> Builder
buildNode (Node n a) = buildBE n <> buildAddr a

data NodeList = NodeList { nodes  :: Maybe [Node IPv4]
                         , nodes6 :: Maybe [Node IPv6]
                         }

decodeNodeList :: BDict B.ByteString -> NodeList
decodeNodeList m = NodeList (go "nodes") (go "nodes6")
  where
    go :: IP a => B.ByteString -> Maybe [Node a]
    go = (`M.lookup` m) >=> asString >=> onlyDo (many' parseNode)

encodeNodeList :: NodeList -> Mappy
encodeNodeList (NodeList n4 n6) = catMaybes [go "nodes" n4, go "nodes6" n6]
  where go str = fmap $ (,) str . BString . mcatmap buildNode

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

mcatmap :: Monoid m => (a -> m) -> [a] -> m
mcatmap = (.) mconcat . map

onlyDo :: Parser a -> B.ByteString -> Maybe a
onlyDo p = maybeResult . (`feed` B.empty) . parse (p <* endOfInput)

