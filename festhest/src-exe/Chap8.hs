{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Chap8 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text
import qualified Data.Text as T
import Data.Time
import Yesod
import Yesod.Form
import Yesod.Form.Jquery

data App where
  App :: App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/person PersonR POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _langs = defaultFormMessage

instance YesodJquery App

data Person where
  Person ::
    { _personName :: Text,
      _personBirthday :: Day,
      _personFavoriteColor :: Maybe Text,
      _personEmail :: Text,
      _personWebsite :: Maybe Text
    } ->
    Person
  deriving (Show)

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs m
  where
    m :: AForm Handler Person
    m = do
      _personName <- areq textField "Name" Nothing
      _personBirthday <-
        areq
          ( jqueryDayField
              def
                { jdsChangeYear = True,
                  jdsYearRange = "1900:-5"
                }
          )
          "Birthday"
          Nothing
      _personFavoriteColor <- aopt textField "Favorite color" Nothing
      _personEmail <- areq emailField "Email address" Nothing
      _personWebsite <- aopt urlField "Website" Nothing
      pure $ Person {..}

postPersonR :: Handler Html
postPersonR = do
  ((result, widget), enctype) <- runFormPost personForm
  case result of
    FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
    _ ->
      defaultLayout
        [whamlet|
          <p>Invalid input, please try again.
          <form method=post action=@{PersonR} enctype=#{enctype}>
            ^{widget}
            <button>Submit
        |]

getHomeR :: Handler Html
getHomeR = do
  (widget, enctype) <- generateFormPost personForm
  defaultLayout do
    setTitle "Forms"
    [whamlet|
      <p>
        The widget geenrated contains only the contents
        of the form, not the form tag itself. So...
      <form method=post action=@{PersonR} enctype=#{enctype}>
        ^{widget}
        <p>It also doesn't include the submit button
        <button>Submit
    |]
