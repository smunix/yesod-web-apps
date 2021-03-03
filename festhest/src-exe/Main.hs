{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Main where

import qualified Chap7
import qualified Chap8
import qualified Control.Concurrent.Async as Async
import Control.Monad ((>=>))
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BL
import Data.Function
import qualified Error
import Message (Message (Message))
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Slash (Slash (Slash))
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

instance Yesod App where
  defaultLayout w = do
    pc <- widgetToPageContent do
      w
      toWidget [lucius|body { font-family: verdana }|]
      [whamlet|
        <footer>
          <p>
            Footer Addition (Copyright???)
      |]
    withUrlRenderer
      [hamlet|
          $doctype 5
          <html>
            <head>
              <title>#{pageTitle pc}
              <meta charset=utf-8>
              ^{pageHead pc}
            <body>
              <figure>
                ^{pageBody pc}
        |]

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET
    /some/path SomePathR GET
  |]

getSomePathR :: Handler Html
getSomePathR = defaultLayout page
  where
    page = do
      setTitle "SomePath"
      [whamlet|<p>SomePath|]

getHomeR :: Handler Html
getHomeR = defaultLayout page
  where
    footer :: Widget
    footer =
      toWidget
        [hamlet|
           <footer>
             <p>That's all folks!
        |]

    page :: Widget
    page = do
      setTitle "Html"
      [whamlet|
          <p>
             This is my page. I hope you enjoyed it.
          ^{footer}
        |]

main :: IO ()
main = do
  putStrLn "Starting Warps 80 / 81 App..."
  [ warp 3080 App,
    warp 3081 Slash,
    warp 3082 Message,
    warp 3083 Error.App,
    warp 3087 Chap7.App,
    warp 3088 Chap8.App
    ]
    & Async.mapConcurrently_ id
  putStrLn "Ending Warp 80 / 81 App..."
