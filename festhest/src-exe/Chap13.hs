-- |
module Chap13 where

import Control.Monad
import Control.Monad.Logger
import Data.ByteString.Base64
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text
import Data.Text.Encoding
import Data.Time
import Database.Persist.Sqlite
import System.Random
import System.Random.Stateful
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Link
  title Text
  url Text
  added UTCTime
|]

data App where
  App ::
    { cpool :: ConnectionPool,
      rgen :: AtomicGenM StdGen
    } ->
    App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/add-link AddLinkR POST
|]

instance Yesod App where
  makeSessionBackend _ = defaultClientSessionBackend 1 "chap-13.aes" <&> pure

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = getYesod >>= runSqlPool action . cpool

getHomeR :: HandlerFor App Html
getHomeR =
  defaultLayout
    [whamlet|
  <form method=post action=@{AddLinkR}>
    <p>
      Add a new link to
      <input type=url name=url value=http://>
      titled
      <input type=text name=title>
      <input type=submit value="Add Link">
  <hr>
  <h2>Existing links
  ^{existingLinks}
|]

existingLinks :: WidgetFor App ()
existingLinks = do
  links <- selectList [] [LimitTo 5, Desc LinkAdded] & runDB & handlerToWidget
  rs <- replicateM (Prelude.length links) rand
  [whamlet|
    <ol>
      $forall (r, Entity _ link) <- Prelude.zip rs links
        <li>
          <a href=#{linkUrl link} target=_blank>#{linkTitle link} (#{show $ linkAdded link}) (rand: #{r})
  |]
  where
    rand :: WidgetFor App Text
    rand = do
      g <- rgen <$> getYesod
      l <- uniformRM (1, 10) g
      uniformByteStringM l g <&> decodeUtf8 . encode

postAddLinkR :: HandlerFor App ()
postAddLinkR = do
  url <- ireq urlField "url" & runInputPost
  title <- ireq textField "title" & runInputPost
  added <- getCurrentTime & liftIO
  Link title url added & insert & runDB
  setMessage "Link added"
  redirect HomeR

run :: Int -> IO ()
run port = runStderrLoggingT $ withSqlitePool "chap-13.db" 10 \pool -> liftIO do
  runSqlPersistMPool (runMigration migrateAll) pool
  gen <- newStdGen >>= newAtomicGenM
  App pool gen & warp port
