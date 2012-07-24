{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Types 
    ( Element(..)
    , DocValue(..)
    , Doc(..)
    , Docs(..)    
    , Change(..)
    , Results(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Explicit

data Element = Element {
    elemUrl :: String
,   elemRegPositive :: [String]
,   elemRegNegative :: [String]
,   elemStatus :: Bool
} deriving (Show)

instance FromJSON Element where
    parseJSON (Object v) = Element <$> 
                            v .: "url" <*>
                            v .: "reg_positive" <*>
                            v .: "reg_negative" <*>
                            v .: "status" 
    parseJSON _ = mzero

instance ToJSON Element where
    toJSON (Element url rp rn status) = object [ "url" .= url
                                               , "reg_positive" .= rp
                                               , "reg_negative" .= rn
                                               , "status" .= status]

--------------------------------------

data DocValue = DocValue {
    docRev :: String
} deriving (Show)

instance FromJSON DocValue where
    parseJSON (Object v) = DocValue <$> v .: "rev"

--------------------------------------

data Doc = Doc {
    docId :: String
,   docKey :: Path
,   docValue :: DocValue      
} deriving (Show)

instance FromJSON Doc where
    parseJSON (Object v) = Doc <$>
                            v .: "id" <*>
                            v .: "key" <*>
                            v .: "value" 
    parseJSON _ = mzero

--------------------------------------

data Docs = Docs {
    docsTotalRows :: Int
,   docsOffset :: Int
,   docs :: [Doc]
} deriving (Show)

instance FromJSON Docs where
    parseJSON (Object v) = Docs <$>
                            v .: "total_rows" <*>
                            v .: "offset" <*>
                            v .: "rows"

--------------------------------------

data Results = Results {
    results :: [Change]
} deriving (Show)

instance FromJSON Results where
    parseJSON (Object v) = Results <$> v .: "results"

--------------------------------------

data Change = Change {
    chSeq :: Int
,   chId :: String
,   chChanges :: [DocValue]
,   chDeleted :: Maybe Bool
} deriving (Show)

instance FromJSON Change where
    parseJSON (Object v) = Change <$> 
                            v .: "seq" <*>
                            v .: "id"  <*>
                            v .: "changes" <*>
                            v .:? "deleted"
