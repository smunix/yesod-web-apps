module Main where

import Yesod

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
  warp 80 App
