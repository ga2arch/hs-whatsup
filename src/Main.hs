{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}

module Main where

import Prelude hiding (catch)

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Explicit
import Network.HTTP
import Network.Browser
import Text.RegexPR
import System.Timeout

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L

import Couch
import Types 

main :: IO ()
main = do
    runCouch def $ do
        {--j <- couchGetAllDocs "elements"
        --couchPut "elements" "google" "" [] $ 
        --            Element "http://www.google.it" [] [] True
        l <- mapM (\e -> do 
            (r, elem) <- couchGet "elements" (docKey e) []
            s <- liftIO $ processUrl elem
            return s) (docs j)
        liftIO $ print l--}
        couchChanges "elements" (liftIO . print) 
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
    handleIO ex = return Nothing

checkUrl :: String -> IO (Maybe String)
checkUrl url = do
    (_, rsp) <- browse $ do
        setAllowRedirects True
        setOutHandler (const $ return ())
        setErrHandler (const $ return ())
        request $ getRequest url
    return $ Just (rspBody rsp)

checkReggie :: String -> [String] -> [String] -> Bool
checkReggie content pos neg = null p && null n
  where
    p = filter (==False) $ map (checkRegex content) pos
    n = filter (==True)  $ map (checkRegex content) neg

checkRegex :: String -> String -> Bool
checkRegex content regex = 
    case (matchRegexPR regex content) of
        Just _  -> True
        Nothing -> False