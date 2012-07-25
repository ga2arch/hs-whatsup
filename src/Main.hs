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
import Network.HTTP.Types.Status

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

    _ <- case s of 
            Right _   -> updateDoc $ el { elOnline = True
                                        , elLastCheck = t 
                                        , elError = Nothing }
            Left  ex  -> updateDoc $ el { elOnline = False 
                                        , elError = Just ex } 
    putStrLn (show s)
  where
    updateDoc e = runCouch def $ 
        couchPut' "elements" chId [] $ e { elFlag = False }

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
                                            "StatusCodeException" $ Just $
                                            (show $ statusCode s) 
                                            ++ " - " ++ 
                                            (S.unpack $ statusMessage s)
            InvalidUrlException url err -> HttpError 
                                            "InvalidUrlException" $ Just $
                                            err ++ " - " ++ url
            HttpParserException s       -> HttpError
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