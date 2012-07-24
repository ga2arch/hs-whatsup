{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS -Wall #-}

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

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

data Element = Element {
    elUrl :: L.ByteString
,   elRegPositive :: [S.ByteString]
,   elRegNegative :: [S.ByteString]
,   elStatus :: Bool
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
    docRev :: L.ByteString
} deriving (Show)

instance FromJSON DocValue where
    parseJSON (Object v) = DocValue <$> v .: "rev"
    parseJSON _ = mzero

--------------------------------------

data Doc = Doc {
    docId :: S.ByteString
,   docKey :: S.ByteString
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
    parseJSON _ = mzero

--------------------------------------

data Results = Results {
    results :: [Change]
} deriving (Show)

instance FromJSON Results where
    parseJSON (Object v) = Results <$> v .: "results"
    parseJSON _ = mzero

--------------------------------------

data Change = Change {
    chSeq :: Int
,   chId :: S.ByteString
,   chChanges :: [DocValue]
,   chDeleted :: Maybe Bool
} deriving (Show)

instance FromJSON Change where
    parseJSON (Object v) = Change <$> 
                            v .: "seq" <*>
                            v .: "id"  <*>
                            v .: "changes" <*>
                            v .:? "deleted"
    parseJSON _ = mzero
