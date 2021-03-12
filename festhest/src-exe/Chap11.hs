{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
module Chap11 where

import Data.Proxy (Proxy (Proxy))
import Data.Text
import Database.Persist.Sqlite
import GHC.Generics
import Optics
import Yesod

data Person where
  Person ::
    { name :: Text,
      age :: Int
    } ->
    Person
  deriving (Show, Generic, ToJSON, FromJSON)

data HaskellShow where
  HaskellShow :: forall a. (Show a) => a -> HaskellShow

instance ToContent HaskellShow where
  toContent (HaskellShow a) = toContent $ show a

instance ToTypedContent HaskellShow where
  toTypedContent = TypedContent (getContentType (Proxy @HaskellShow)) . toContent

instance HasContentType HaskellShow where
  getContentType _ = "text/haskell-show"

data App where
  App :: App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod App where
  makeSessionBackend _ = defaultClientSessionBackend 1 "chap-11.aes" <&> pure

getHomeR :: HandlerFor App TypedContent
getHomeR = selectRep do
  provideRep $ pure $ HaskellShow p
  provideRep $
    defaultLayout
      [whamlet|
        <p>Hello, my name is #{name}, and I am #{age} years old.
      |]
  provideJson p
  where
    name = "Providence"
    age = 30
    p = Person {..}

run :: Int -> IO ()
run port = warp port App
