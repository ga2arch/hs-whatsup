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
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Time.Clock
import Database.CouchDB.Conduit
import Database.CouchDB.Conduit.Explicit
import Text.Regex.PCRE.Light
import Network.HTTP.Conduit
import Network.HTTP.Types.Status

import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L

-- std
import Data.Maybe

main :: IO ()
main = do
    runCouch def $ do 
        source <- couchContinuousChanges "elements"
        source $$ sinkChanges

sinkChanges :: MonadCouch m => Pipe l Change o r m r
sinkChanges = 
    awaitForever $ \Change{..} -> do
        _ <- liftIO . forkIO $ do
            r <-  try $ runCouch def (couchGet "elements" chId [])
            case r of
                Left (CouchHttpError a _)   -> putStrLn (show a)
                Left (CouchInternalError e) -> putStrLn (show e)
                Left (NotModified)          -> putStrLn "Not modified"
                Right (_, el)               -> processElement chId el
        return ()

processElement :: S.ByteString -> Element -> IO ()
processElement chId el@Element{..} = do
    c <- getCurrentTime

    when (isJust elNextCheck) $ 
        threadDelay $ subn (fromJust elNextCheck) c
    updateElement chId el
  where
    subn (NextCheck nc) cu | nc > cu = (round $ diffUTCTime nc cu) * 1000000
                           | otherwise = 0

updateElement :: S.ByteString -> Element -> IO ()
updateElement chId el = do
    s <- processUrl el
    _ <- case s of 
            Right _ -> 
                updateDoc $ 
                    el { elSuccess = True
                       , elError = Nothing }

            Left ex -> 
                updateDoc $ 
                    el { elSuccess = False 
                       , elError = Just ex } 

    putStrLn (show s)
  where
    updateDoc e = do 
        nc <- liftM (NextCheck . addUTCTime (5)) getCurrentTime
        runCouch def $ 
            couchPut' "elements" chId [] $ e { elNextCheck = Just nc }

processUrl :: Element -> IO (Either CheckError ())
processUrl Element{..} = do
    body <- (checkUrl elUrl) 
        `catches` [ Handler (\(ex :: IOException) -> handleIO ex)
                  , Handler (\(ex :: HttpException) -> handleHttp ex)]
    return $ body >>= checkRegexes elRegPositive elRegNegative
  where
    handleIO   ex = return . Left . IOError $ show ex
    handleHttp ex = return . Left $ 
        case ex of 
            StatusCodeException s _     -> HttpError
                "StatusCodeException" . Just $
                (show $ statusCode s) ++ " - " ++ (S.unpack $ statusMessage s)

            InvalidUrlException url err -> HttpError 
                "InvalidUrlException" . Just $
                err ++ " - " ++ url

            HttpParserException s       -> 
                HttpError
                "HttpParserException" $ Just s
            _                           -> HttpError (show ex) Nothing
 
checkUrl :: L.ByteString -> IO (Either CheckError L.ByteString)
checkUrl url = liftM Right $ simpleHttp (L.unpack url)

checkRegexes :: [S.ByteString] 
                -> [S.ByteString] 
                -> L.ByteString
                -> Either CheckError ()
checkRegexes pos neg content = 
    if (null p && null n) 
        then return ()
        else Left $ RegexError pos neg
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