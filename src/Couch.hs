{-# LANGUAGE OverloadedStrings #-}
module Couch where 

import Control.Exception
import Data.Aeson
import Data.Conduit
import Data.Void
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.DB
import Database.CouchDB.Conduit.Generic
import Database.CouchDB.Conduit.LowLevel
import Data.String.Conversions ((<>), cs)
import Text.Printf

import qualified Data.Aeson.Generic as AG
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types   as HT

import Types

couchGetAllDocs :: MonadCouch m => Path -> m (Docs)
couchGetAllDocs db = do
    H.Response _ _ _ bsrc <- couch HT.methodGet (mkPath [db, "_all_docs"]) [] [] 
                                    (H.RequestBodyBS S.empty) protect'
    j <- bsrc $$+- CA.sinkParser json
    o <- jsonToTypeWith fromJSON j
    return o

couchChanges :: MonadCouch m => 
                Path -> (Change -> Pipe Path Path Void () m r') -> m ()
couchChanges db cb = do
    H.Response _ _ _ bsrc <- couch HT.methodGet path [] []
                                (H.RequestBodyBS S.empty) protect'
    bsrc $$+- sink
  where
    toLazy x = L.fromChunks $ [x]
    path = S.append db "/_changes/?feed=continuous"
    sink = CB.lines =$= (awaitForever processInput)
    processInput input = do
        let (Just ch) = decode (toLazy input) :: Maybe Change
        cb ch


jsonToTypeWith :: MonadResource m => (Value -> Result a) -> Value -> m a
jsonToTypeWith f j = case f j of
        Error e -> throw $ CouchInternalError $ 
                        "Error parsing json: " <> cs e
        Success o -> return o