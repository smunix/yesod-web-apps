{-# LANGUAGE EmptyCase #-}

module Main where

import Control.Monad ((>=>))
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import Data.Function
import Network.HTTP.Types (status200, status404)
import Network.Wai
import System.IO (IOMode (ReadMode), withBinaryFile)
import Yesod

type Send' = Response -> IO ResponseReceived

type Application' = Request -> Send' -> IO ResponseReceived

simpleRun :: Int -> Application' -> IO ()
simpleRun = error "nyi"

bigFile :: IO ()
bigFile = do
  simpleRun 80 \_req _send ->
    withBinaryFile
      "big-file.csv"
      ReadMode
      ( BL.hGetContents
          >=> _send
            . responseLBS
              status200
              [("Content-Type", "text/csv; charset=utf-8")]
      )

routing :: IO ()
routing = simpleRun 80 \req send -> do
  let ok200 :: Builder -> IO ResponseReceived
      ok200 = send . responseBuilder status200 []
      fail404 :: Builder -> IO ResponseReceived
      fail404 = send . responseBuilder status404 []
  case pathInfo req of
    [] -> ok200 "Home page"
    ["foo"] -> ok200 "/foo"
    ["foo", "bar"] -> ok200 "/foo/bar"
    pti -> fail404 ": Not Found"

chaos :: Middleware
chaos app req send = do
  let newReq = req {pathInfo = "marauder" : pathInfo req}
  r <- app newReq send
  putStrLn "Mischief managed"
  pure r

vhost :: Application -> Application -> IO ()
vhost org com = simpleRun
  80
  \req send -> do
    let s = send . responseBuilder status200 []
        f = send . responseBuilder status404 []
    case req & requestHeaders & lookup "host" of
      Just "www.example.org" -> org req send
      Just "www.example.com" -> com req send
      Just host -> byteString host & f
      _ -> f "No host"

data App = App

instance Yesod App

mkYesod
  "App"
  [parseRoutes|
              / HomeR GET
              |]

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet| <h1>Hello, Haskell fanatics! |]

main :: IO ()
main = do
  putStrLn "Starting Warp 80 App..."
  warp 3080 App
