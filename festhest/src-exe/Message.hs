{-# LANGUAGE RecordWildCards #-}

-- |
module Message where

import Data.Function
import qualified Data.Time as Time
import Yesod

data Message where
  Message :: Message

mkYesod
  "Message"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod Message where
  defaultLayout w = do
    PageContent {..} <- widgetToPageContent w
    mmsg <- getMessage
    withUrlRenderer
      [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle}
          <meta charset="utf8">
        <body>
          $maybe msg <- mmsg
            <div #message>#{msg}
          ^{pageBody}
      |]

getHomeR :: Handler Html
getHomeR = do
  now <- liftIO Time.getCurrentTime
  ("You previously visited at: " <> show now)
    & toHtml
    & setMessage
  defaultLayout [whamlet|<p>Try refreshing|]
