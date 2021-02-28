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
    provideRep $ defaultLayout do
      let handlerName = "getGreetR" :: Text
      setTitle "Greetings There!"
      [whamlet|<p>Greetings to my folks - #{name} - from #{handlerName}|]
