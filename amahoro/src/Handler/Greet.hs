{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
module Handler.Greet where

import Import

getGreetR :: Text -> Handler TypedContent
getGreetR name =
  selectRep do
    provideRep $
      pure $
        object
          ["name" .= name]
    provideRep $ greetLayout do
      let handlerName = "getGreetR" :: Text
      setTitle "Greetings (greetLayout) There!"
      [whamlet|<p>Greetings to my folks - #{name} - from #{handlerName}|]

greetLayout :: Widget -> Handler Html
greetLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer
    [hamlet|
           $doctype 5
           <html>
             <head>
               <title>#{pageTitle pc}
               <meta charset=utf-8>
               <style>body { font-family: verdana }
               ^{pageHead pc}
             <body>
               <article>
                 ^{pageBody pc}
           |]
