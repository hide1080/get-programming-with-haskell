module Main (main) where

import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
--Q39-2
import Network.HTTP.Types.Status

--L39-4
main :: IO ()
main = do
  token <- BC.pack <$> getEnv "NOAA_API_TOKEN"
  let request = buildRequest token noaaHost method apiPath
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
  then do
    print "saving request to file"
    let jsonBody = getResponseBody response
    L.writeFile "data.json" jsonBody
  else do
    --Q39-2
    let responseStatus = getResponseStatus response
    print $ statusCode responseStatus
    print $ statusMessage responseStatus

--L39-1 API TOKEN (https://www.ncdc.noaa.gov/cdo-web/token)
myToken :: BC.ByteString
myToken = "..."

-- www.ncdc.noaa.govからホスト名が変更された
noaaHost :: BC.ByteString
noaaHost = "www.ncei.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

method :: BC.ByteString
method = "GET"

--QC39-1
--ファイル先頭に以下を書く（LANGUAGEプラグマ）
--{-# LANGUAGE OverloadedStrings #-}

--QC39-2
-- 1
-- ghci> import Network.HTTP.Simple
-- ghci> response = httpLBS "http://news.ycombinator.com"
-- ghci> getResponseHeaders <$> response
-- 2
-- ghci> import Network.HTTP.Simple
-- ghci> response <- httpLBS "http://news.ycombinator.com"
-- ghci> getResponseHeaders response

--L39-5
buildRequest :: BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> BC.ByteString
             -> Request
buildRequest token host method path
  = setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

--Q39-1
buildRequestNoSSL :: BC.ByteString
                  -> BC.ByteString
                  -> BC.ByteString
                  -> BC.ByteString
                  -> Request
buildRequestNoSSL token host method path
  = setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "token" [token]
  $ setRequestPath path
  $ setRequestSecure False
  $ setRequestPort 80
  $ defaultRequest

requestNoSSL :: Request
requestNoSSL = buildRequestNoSSL myToken noaaHost "GET" apiPath
