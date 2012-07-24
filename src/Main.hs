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
import Control.Concurrent
import Control.Exception
import Control.Monad
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
    chan <- newChan

    _ <- forkIO . forever $ do
        Change{..} <- readChan chan
        r <- try $ runCouch def (couchGet "elements" chId []) 
        case r of
            Left (CouchHttpError a _) -> putStrLn (show a)
            Left (CouchInternalError e) -> putStrLn (show e)
            Left NotModified -> putStrLn ("Not modified" :: String)
            Right (_, el) -> processUrl el >>= print

    runCouch def $ couchContinuousChanges "elements" chan

processUrl :: Element -> IO (Bool)
processUrl Element{..} = do
    body <- (checkUrl elUrl) 
        `catch` (\(ex :: IOException) -> handleIO ex)
    case body of 
        Just content -> return $ 
                            checkReggie content 
                                        elRegPositive 
                                        elRegNegative
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