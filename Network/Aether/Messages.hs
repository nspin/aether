

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

pbs :: B.ByteString -> BValue IBuilder
pbs = BString . prefix

------------------
-- METHODS
------------------

data Ping = Ping Word160 deriving Show

instance Method Ping where
    name _ = "ping"
    data Response Ping = Pong Word160 deriving Show
    writeQuery (Ping id) = [("id", BString $ prefixBE id)]
    writeResp  (Pong id) = [("id", BString $ prefixBE id)]
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
    writeQuery (FindNode i t w) = [ ("id", BString $ prefixBE i)
                                  , ("target", BString $ prefixBE t)
                                  , ("want", BList $ map (BString . buildWant) w)
                                  ]
    writeResp (FoundNode i ns) = ("id", BString $ prefixBE i) : encodeNodeList ns

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

