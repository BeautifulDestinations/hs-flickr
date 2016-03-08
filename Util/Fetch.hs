--------------------------------------------------------------------
-- |
-- Module    : Util.Fetch
-- Copyright : (c) Sigbjorn Finne, 2008
-- License   : BSD3
--
-- Maintainer: Sigbjorn Finne <sof@forkIO.com>
-- Stability : provisional
-- Portability: so-so
--
-- Simple GET\/de-ref of URLs; abstracting out networking backend\/package.
--
module Util.Fetch
       ( readContentsURL
       , readUserContentsURL

       , postContentsURL

       , URLString
       , User(..)
       ) where

import Control.Applicative
import Control.Exception   (catch, throwIO, toException)

import Network.Browser
import Network.HTTP.Types.Status (Status(..))
import Network.HTTP.Conduit ( parseUrl, Request(..), Response(..), newManager
                            , tlsManagerSettings, responseBody, httpLbs, Manager
                            , HttpException(..))
import Network.HTTP hiding (Request, Response)
import Network.URI

import Data.ByteString.Lazy (pack, toStrict, ByteString)

import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)

type URLString = String

data User = User
  { userName :: String
  , userPass :: String
  }

handleStatusCodeEx :: Int -> Request -> Manager -> HttpException -> IO (Response ByteString)
handleStatusCodeEx n req manager s@(StatusCodeException (Status 504 msg) _ _)
  | n >= 10 = throwIO $ toException s
  | otherwise = do
      putStrLn $ unpack $ decodeUtf8 msg
      httpLbs req manager `catch` handleStatusCodeEx (n+1) req manager
handleStatusCodeEx n _ _ e = throwIO $ toException e

readContentsURL :: URLString -> IO String
readContentsURL url = do
    req <- parseUrl url
    manager <- newManager tlsManagerSettings
    let req' = req { responseTimeout = Just timeout }
    respBody <- responseBody <$> httpLbs req' manager `catch` handleStatusCodeEx 0 req' manager
    return $ unpack $ decodeUtf8 $ toStrict respBody
  where
    timeout = 120000000 -- 2 mins

readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL usr us = do -- readContentsURL u
  req <-
    case parseURI us of
      Nothing -> fail ("ill-formed URL: " ++ us)
      Just ur -> return (defaultGETRequest ur)
    -- don't like doing this, but HTTP is awfully chatty re: cookie handling..
  let nullHandler _ = return ()
  (u, resp) <- browse $ do
      setOutHandler nullHandler
      setAllowBasicAuth True
      setAuthorityGen (\ _ _ -> return (Just (userName usr,userPass usr)))
      request req
  case rspCode resp of
    (2,_,_) -> return (rspBody resp)
    _ -> fail ("Failed reading URL " ++ show u ++ " code: " ++ show (rspCode resp))


postContentsURL :: URLString -> [(String,String)] -> String -> IO String
postContentsURL u hdrs body = do
  let hs =
       case parseHeaders $ map (\ (x,y) -> x++": " ++ y) hdrs of
         Left{} -> []
         Right xs -> xs
  req0 <-
    case parseURI u of
      Nothing -> fail ("ill-formed URL: " ++ u)
      Just ur -> return (defaultGETRequest ur)
  let req = req0{
      rqMethod=POST
    , rqBody=body
    , rqHeaders=hs
  }
  let nullHandler _ = return ()
  (_,rsp) <- browse $ setOutHandler nullHandler >> request req
  case rspCode rsp of
    (2,_,_) -> return (rspBody rsp)
    x -> fail ("POST failed - code: " ++ show x ++ ", URL: " ++ u)

{- Curl versions:
readUserContentsURL :: User -> URLString -> IO String
readUserContentsURL u url = do
  let opts = [ CurlHttpAuth [HttpAuthAny]
             , CurlUserPwd (userName u ++
                        case userPass u of {"" -> ""; p -> ':':p })
             , CurlFollowLocation True
         ]
  (_,xs) <- curlGetString url opts
  return xs

postContentsURL :: URLString -> [(String,String)] -> String -> IO String
postContentsURL u hdrs body = do
  let opts = [ CurlCustomRequest "POST"
             , CurlFollowLocation True
         , CurlPost True
         , CurlPostFields [body]
         , CurlHttpTransferDecoding False
         ] ++ [CurlHttpHeaders (map ( \ (x,y) -> (x ++ ':':y)) hdrs)]
  rsp <- curlGetResponse u opts
  case respStatus rsp `div` 100 of
    2 -> return (respBody rsp)
    x -> fail ("POST failed - code: " ++ show x ++ ", URL: " ++ u)

-}
