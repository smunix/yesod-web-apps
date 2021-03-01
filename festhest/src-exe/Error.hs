-- |
module Error where

import Data.Function
import Data.Functor
import Yesod

data App where
  App :: App

instance Yesod App where
  errorHandler NotFound = errM & defaultLayout <&> toTypedContent
    where
      errM = do
        setTitle "Request page not located"
        [whamlet|
                    <h1>NotFound
                    <p>
                      We apologize for the incovenience, but the requested page could not be located.
                    |]
  errorHandler e = defaultErrorHandler e

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/error ErrorR GET
/not-found NotFoundR GET
|]

getHomeR :: Handler Html
getHomeR =
  defaultLayout
    [whamlet|
        <p>
          <a href=@{ErrorR}>Internal server error
        <p>
          <a href=@{NotFoundR}>Not found
|]

getNotFoundR :: Handler Html
getNotFoundR = notFound

getErrorR :: Handler Html
getErrorR = error "nyi"
