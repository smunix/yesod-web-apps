module Main where

import Yesod

data App = App

instance Yesod App

mkYesod "App" [parseRoutes| HomeR GET |]
  
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet| <h1>Hello, Haskell funs! |]
  
main :: IO ()
main = putStrLn "Hello, Haskell!"
