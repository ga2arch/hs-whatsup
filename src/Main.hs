{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main where

-- local
import Couch
import Types

-- GHC 
import Prelude hiding (catch)

-- libraries
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Explicit
import Text.Regex.PCRE.Light
import Network.HTTP.Conduit

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L

-- std
import Data.Maybe

main :: IO ()
main = do
    runCouch def $ do
        j <- couchGetAllDocs "elements"
        --couchPut "elements" "google" "" [] $ 
        --            Element "http://www.google.it" [] [] True
        --(r, elem) <- couchGet "elements" "google" []
        l <- mapM (\e -> do 
            (_, el) <- couchGet "elements" (docKey e) []
            s <- liftIO $ processUrl el
            return s) (docs j)
        liftIO $ print l
        --couchChanges "elements" (liftIO . print) 
        return ()

processUrl :: Element -> IO (Bool)
processUrl Element{..} = do
    body <- (checkUrl elemUrl) 
        `catch` (\(ex :: IOException) -> handleIO ex)
    case body of 
        Just content -> return $ 
                            checkReggie content 
                                        elemRegPositive 
                                        elemRegNegative
        Nothing      -> return False
  where
    handleIO _ = return Nothing

checkUrl :: L.ByteString -> IO (Maybe L.ByteString)
checkUrl url = simpleHttp (L.unpack url) >>= return . Just 

checkReggie :: L.ByteString -> [S.ByteString] -> [S.ByteString] -> Bool
checkReggie content pos neg = null p && null n
  where
    p = filter (==False) $ map (checkRegex content) pos
    n = filter (==True)  $ map (checkRegex content) neg

checkRegex :: L.ByteString -> S.ByteString -> Bool
checkRegex content regex = 
    case rex of 
        Left  _ -> False
        Right r -> isJust $ match r (toStrict content) [] 
  where
    rex = compileM regex [] 

toStrict :: L.ByteString -> S.ByteString
toStrict = S.concat . L.toChunks 