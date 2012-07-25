{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Types 
    ( Element(..)
    , DocValue(..)
    , Doc(..)
    , Docs(..)    
    , Change(..)
--   , Results(..)
    , CheckError(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM

data Element = Element {
    elUrl :: L.ByteString
,   elRegPositive :: [S.ByteString]
,   elRegNegative :: [S.ByteString]
,   elOnline :: Bool
,   elFlag :: Bool
,   elLastCheck :: Integer
,   elError :: Maybe CheckError
} deriving (Show)

instance FromJSON Element where
    parseJSON (Object v) = Element <$> 
                            v .: "url" <*>
                            v .:? "reg_positive" .!= [] <*>
                            v .:? "reg_negative" .!= [] <*>
                            v .:? "online" .!= False <*>
                            v .:? "flag"   .!= True  <*>
                            v .:? "last_check" .!= 0 <*>
                            v .:? "error".!= Nothing
    parseJSON _ = mzero

instance ToJSON Element where
    toJSON (Element url rp rn online flag lc err) = 
        object [ "url" .= url
               , "reg_positive" .= rp
               , "reg_negative" .= rn
               , "online" .= online
               , "flag" .= flag
               , "last_check" .= lc
               , "error" .= err]

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

data CheckError = HttpError  { heException :: String
                             , heMessage :: Maybe String }
                | IOError    { ioException :: String }
                | RegexError { rePositive :: [S.ByteString] 
                             , reNegative :: [S.ByteString] }
    deriving (Show)

instance FromJSON CheckError where
    parseJSON (Object v) = 
        case (HM.lookup "type" v) of
            Just "http"  -> HttpError <$>
                              v .: "exception" <*>
                              v .:? "message" .!= Nothing
            Just "io"    -> IOError <$>
                              v .: "exception"
            Just "regex" -> RegexError <$>
                              v .: "reg_positive" <*>
                              v .: "reg_negative"
            Just _        -> mzero
            Nothing       -> mzero
    parseJSON _ = mzero

instance ToJSON CheckError where 
    toJSON (HttpError{..}) = object [ "type" .= ("http" :: String)
                                    , "exception" .= heException
                                    , "message" .= heMessage ]
    toJSON (IOError{..}) = object [ "type" .= ("io"  :: String)
                                  , "exception" .= ioException ]
    toJSON (RegexError{..}) = object [ "type" .= ("regex" :: String)
                                     , "reg_positive" .= rePositive 
                                     , "reg_negative" .= reNegative]

{--------------------------------------

data Results = Results {
    results :: [Change]
} deriving (Show)

instance FromJSON Results where
    parseJSON (Object v) = Results <$> v .: "results"
    parseJSON _ = mzero

--------------------------------------}
