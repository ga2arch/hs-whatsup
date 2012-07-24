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
import Data.Time.Clock.POSIX
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
            Left (CouchHttpError a _)   -> putStrLn (show a)
            Left (CouchInternalError e) -> putStrLn (show e)
            Left (NotModified)          -> putStrLn "Not modified"
            Right (_, el)               -> if (elFlag el) 
                                            then updateElement chId el
                                            else return ()

    runCouch def $ couchContinuousChanges "elements" chan

updateElement :: S.ByteString -> Element -> IO ()
updateElement chId el = do
    t <- liftM floor getPOSIXTime
    s <- processUrl el
    _ <- runCouch def $ couchPut' "elements" chId [] $ 
                            el { elOnline = s 
                               , elFlag = False
                               , elLastCheck = t }
    putStrLn (show s)

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
checkUrl url = liftM Just $ simpleHttp (L.unpack url)

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