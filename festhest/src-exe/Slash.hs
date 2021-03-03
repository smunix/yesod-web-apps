-- |
module Slash where

import Blaze.ByteString.Builder.Char.Utf8
import Control.Arrow
import Data.ByteString
import Data.Function
import Data.Functor
import Data.Text
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Yesod

data Slash where
  Slash :: Slash

mkYesod
  "Slash"
  [parseRoutes|
              / SlashHomeR GET
              /foo SlashFooR GET
  |]

getSlashHomeR :: Handler Html
getSlashHomeR = defaultLayout do
  [whamlet|
          <p>
            <a href=@{SlashHomeR}>SlashHomeR
          <p>
            <a href=@{SlashFooR}>SlashFooR

          |]

getSlashFooR :: Handler Html
getSlashFooR = getSlashHomeR

instance Yesod Slash where
  joinPath _ ar pieces qs' =
    fromText ar
      <> encodePath
        (pieces <> [""])
        ( qs'
            <&> ( TE.encodeUtf8 *** \case
                    "" -> Nothing
                    x -> x & TE.encodeUtf8 & pure
                )
        )
