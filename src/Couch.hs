{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Couch 
    ( couchGetAllDocs
    , couchContinuousChanges
    ) where 

-- friends
import Types

-- libraries
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.Aeson
import Data.Conduit
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.LowLevel
import Data.String.Conversions ((<>), cs)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified Network.HTTP.Conduit as H
import qualified Network.HTTP.Types   as HT

-- std
-- import Data.Void

couchGetAllDocs :: MonadCouch m => S.ByteString -> m (Docs)
couchGetAllDocs db = do
    H.Response _ _ _ bsrc <- couch HT.methodGet (mkPath [db, "_all_docs"]) [] [] 
                                    (H.RequestBodyBS S.empty) protect'
    j <- bsrc $$+- CA.sinkParser json
    o <- jsonToTypeWith fromJSON j
    return o

couchContinuousChanges :: MonadCouch m => S.ByteString -> Chan Change -> m ()
couchContinuousChanges db chan = do
    H.Response _ _ _ bsrc <- couch HT.methodGet path 
                                [(HT.hConnection, "Keep-Alive")] []
                                (H.RequestBodyBS S.empty) protect'
    bsrc $$+- sink
  where
    toLazy x = L.fromChunks $ [x]
    path = S.append db "/_changes?feed=continuous&heartbeat=3000"
    sink = CB.lines =$= (awaitForever processInput)
    processInput input = do
        let mch = decode (toLazy input) :: Maybe Change
        case mch of 
            Just ch -> liftIO $ writeChan chan ch
            Nothing -> return ()

jsonToTypeWith :: MonadResource m => (Value -> Result a) -> Value -> m a
jsonToTypeWith f j = case f j of
        Error e -> throw $ CouchInternalError $ 
                        "Error parsing json: " <> cs e
        Success o -> return o