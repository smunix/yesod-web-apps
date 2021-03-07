{-# LANGUAGE ApplicativeDo #-}

-- |
module Chap9 where

import Data.Function
import Data.Functor
import Yesod

data App where
  App :: App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET POST
/set-message SetMessageR POST
/sayhello SayHelloR GET
/setname SetNameR GET POST
|]

getHomeR :: Handler Html
getHomeR = do
  sm <- getSession
  defaultLayout do
    [whamlet|
      <form method=post action=@{HomeR}>
        Key<input type=text name=key>
        Value<input type=text name=val>
        <input type=submit>
      <h1>#{show sm}
    |]
    [whamlet|
      <hr>
    |]
    [whamlet|
      <form method=post action=@{SetMessageR}>
        Message #
        <input type=text name=message>
        <input type=submit>
      <h1>#{show sm}
    |]
    [whamlet|
      <hr>
    |]
    [whamlet|
      <p>
        <a href=@{SetNameR}>Set your name
      <p>
        <a href=@{SayHelloR}>Say Hello
    |]
    [whamlet|
      <hr>
    |]

getSayHelloR :: Handler Html
getSayHelloR = do
  mname <- lookupSession "name"
  case mname of
    Just name -> defaultLayout [whamlet|<p>Welcome #{name}...|]
    Nothing -> do
      setUltDestCurrent
      setMessage "Please tell me your name"
      redirect SetNameR

getSetNameR :: Handler Html
getSetNameR =
  defaultLayout
    [whamlet|
      <form method=post action=@{SetNameR}>
        MyName is #
        <input type=text name=name>
        . #
        <input type=submit value="set name">
    |]

postSetNameR :: Handler ()
postSetNameR = do
  ireq textField "name" & runInputPost >>= setSession "name"
  redirectUltDest HomeR

postHomeR :: Handler Html
postHomeR = do
  let fi = do
        k <- ireq textField "key"
        mv <- iopt textField "val"
        return (k, mv)
  (k, mv) <- runInputPost fi
  case mv of
    Nothing -> deleteSession k
    Just val -> setSession k val
  print (k, mv) & liftIO
  redirect HomeR

postSetMessageR :: Handler ()
postSetMessageR = do
  msg <- ireq textField "message" & runInputPost
  msg & setMessage . toHtml
  redirect HomeR

instance Yesod App where
  makeSessionBackend _ = defaultClientSessionBackend 1 "chap-9.aes" <&> pure
  defaultLayout w = do
    pc <- widgetToPageContent w
    mmsg <- getMessage
    mname <- lookupSession "name"
    withUrlRenderer
      [hamlet|
             $doctype 5
             <html>
               <head>
                 <title>#{pageTitle pc}
                 ^{pageHead pc}
               <body>
                 $maybe n <- mname
                   <h1>#{n}
                 $maybe m <- mmsg
                   <p>#{m}
                 ^{pageBody pc}
      |]

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

run :: Int -> IO ()
run port = do
  putStrLn $ "Chap9 => port " <> show port
  App & warp port
