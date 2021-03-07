{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Chap10 where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.Function
import Data.Functor
import Data.Maybe (catMaybes)
import Database.Persist.Sqlite
import Optics
import Yesod

data Sex where
  F :: Sex
  M :: Sex
  deriving (Show, Read, Eq, Enum, Bounded)

-- define our entities
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Person
  firstName String
  lastName String
  age Int
  KeyPerson firstName lastName
  deriving (Show)
Car
  make String
  year Int
  licensePlate String
  KeyCar licensePlate
  deriving (Show)
PersonCar
  personId PersonId
  carId CarId
  KeyPersonCar personId carId
  deriving (Show)
Store
  name String
  address String
  deriving (Show)
PersonStore
  personId PersonId
  storeId StoreId
  KeyPersonStore personId storeId
  deriving (Show)
|]

newtype App where
  App :: {appPool :: ConnectionPool} -> App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/person/#PersonId PersonR GET
|]

instance Yesod App where
  makeSessionBackend _ = defaultClientSessionBackend 1 "chap-10.aes" <&> pure

layout :: (Yesod a) => WidgetFor a () -> HandlerFor a Html
layout w = defaultLayout do
  -- addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.2.4/semantic.min.css"
  addStylesheetRemote "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
  addScriptRemote "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js"
  w

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = getYesod >>= runSqlPool action . appPool

menuR :: ([Entity Person], [Entity Car], [Entity Store]) -> WidgetFor App ()
menuR (persons, cars, stores) =
  [whamlet|
        <div .ui.fixed.inverted.menu>
          <div .ui.container>

            <a .header.item>FestHest Co

            <a .item>Home

            ^{personsMenuW persons}
            ^{carsMenuW cars}
            ^{storesMenuW stores}
      |]

personsMenuW arg =
  [whamlet|
        <div .ui.simple.dropdown.item>
          Persons
          <i .dropdown.icon>
          <div .menu>
            <div.header>Persons
            <div .divider>
            $forall (Entity personid person) <- arg
              <a href=@{PersonR personid}>#{person & personFirstName} #{person & personLastName}
    |]

carsMenuW arg =
  [whamlet|
        <div .ui.simple.dropdown.item>
          Cars
          <i .dropdown.icon>
          <div .menu>
            <div.header>Cars
            <div .divider>
            $forall (Entity carid car) <- arg
              <a>#{car & carMake} (#{car & carYear})
    |]

storesMenuW arg =
  [whamlet|
        <div .ui.simple.dropdown.item>
          Stores
          <i .dropdown.icon>
          <div .menu>
            <div.header>Stores
            <div .divider>
            $forall (Entity storeid store) <- arg
              <a>#{store & storeName}
    |]

getHomeR :: Handler Html
getHomeR =
  db
    ( (,,)
        <$> selectList [] [Asc PersonAge]
        <*> selectList [] [Asc CarMake]
        <*> selectList [] [Asc StoreName]
    )
    \inputs@(persons, cars, stores) -> layout do
      menuR inputs

-- getHomeR :: Handler Html
-- getHomeR =
--   db
--     (selectList [] [Asc PersonAge])
--     \persons ->
--       layout do
--         [whamlet|
--           <div .ui.raised.container.segment>
--             <div .message">
--               <ol>
--                 $forall personEntity@(Entity personid person) <- persons
--                   <li>
--                     <a href=@{PersonR personid}>#{person & personFirstName} #{person & personLastName}
--         |]

getPersonR :: Key Person -> Handler Html
getPersonR keyPerson =
  db
    ( do
        persons <- selectList [] [Asc PersonAge]
        cars <- selectList [] [Asc CarMake]
        stores <- selectList [] [Asc StoreName]

        personcars <-
          selectList [PersonCarPersonId ==. keyPerson] []
            >>= traverseOf traversed \(Entity _ (PersonCar _ x)) -> get x

        personstores <-
          selectList [PersonStorePersonId ==. keyPerson] []
            >>= traverseOf traversed \(Entity _ (PersonStore _ x)) -> get x

        mperson <- get keyPerson

        pinfo <- mperson <&> (,catMaybes personcars,catMaybes personstores) & return

        let menuinfo = (persons, cars, stores)

        return (menuinfo, pinfo)
    )
    \(menuinfo, pinfo) -> layout do
      [whamlet|
        <div .ui.container>
          ^{menuR menuinfo}
          <br>
          ^{personWidget pinfo}
      |]

personWidget :: Maybe (Person, [] Car, [] Store) -> WidgetFor App ()
personWidget (Just (person, cars, stores)) =
  [whamlet|
      <h2 .ui.message.info.header>#{person & personFirstName} #{person & personLastName}
      <div .ui.two.column.grid>
        <div .column>
          <div .top.attached.ui.segments>
            <div .top.attached.ui.segment>
              <p>Cars
            <div .ui.segments>
              $forall car <- cars
                <div .ui.horizontal.segments>
                  <div .ui.segment>
                    <p>#{car & carMake}
                  <div .ui.segment>
                    <p>#{car & carYear}
                  <div .ui.segment>
                    <p>#{car & carLicensePlate}

        <div .column>
          <div .top.attached.ui.segments>
            <div .top.attached.ui.segment>
              <p>Visited stores
            <div .ui.segments>
              $forall store <- stores
                <div .ui.horizontal.segments>
                  <div .ui.segment>
                    <p>#{store & storeName}
  |]

emptyPersonWidget :: WidgetFor App ()
emptyPersonWidget =
  [whamlet|
    <div .ui.message.warning>
      <p>The user could not be found
  |]

getPerson2R :: Key Person -> Handler Html
getPerson2R keyPerson = db
  ( do
      mperson <- get keyPerson

      personcars <- selectList [PersonCarPersonId ==. keyPerson] []
      carms <- personcars & traverseOf traversed \(Entity _ (PersonCar _ carid)) -> get carid

      personstores <- selectList [PersonStorePersonId ==. keyPerson] []
      storems <- personstores & traverseOf traversed \(Entity _ (PersonStore _ storeid)) -> get storeid
      (,catMaybes carms,catMaybes storems) <$> mperson & return
  )
  \pinfos ->
    layout do
      setTitle "Personal Information"
      [whamlet|
        $maybe (person, cars, stores) <- pinfos
          <h2>#{person & personFirstName} #{person & personLastName}
          <hr>
          <h3>Own cars
          <ol>
            $forall car <- cars
              <li>
                <div>
                  <div>#{car & carMake} (#{car & carYear}) #{car & carLicensePlate}
          <h3>Visited stores
          <ol>
            $forall store <- stores
              <li>
                <div>
                  <div>#{store & storeName} at #{store & storeAddress}

      |]

db ::
  -- |
  ReaderT (YesodPersistBackend App) Handler a ->
  -- |
  (a -> Handler b) ->
  Handler b
db action kont = runDB action >>= kont

run :: Int -> IO ()
run port =
  runStderrLoggingT $ withSqlitePool
    "chap-10-sqlite.db3"
    openConnectionCount
    \pool -> liftIO do
      unit
        & flip runSqlPool pool
        & runResourceT
      putStrLn $ "Chap10 => port " <> show port
      App pool & warp port
  where
    openConnectionCount = 10
    unit :: ReaderT SqlBackend (ResourceT IO) ()
    unit = pure ()
    actions :: ReaderT SqlBackend (ResourceT IO) ()
    actions = void do
      runMigration migrateAll
      costco <- Store "Costco" "Av. Des Tourelles, Boisbriand" & insert
      superC <- Store "Super C" "Blv des Promenades, Ste Marthe Sur Le Lac" & insert
      maxi <- Store "Maxi" "Blv des Promenades, Ste Marthe Sur Le Lac" & insert
      mars <- Store "Marsians" "Solar System" & insert
      providence <- Person "Providence" "Salumu" 40 & insert
      nelly <- Person "Nelly" "Arakaza" 40 & insert
      kayla <- Person "Kayla" "Salumu Arakaza" 10 & insert
      kaylee <- Person "Kaylee" "Arakaza Salumu" 1 & insert
      venza <- Car "Venza Toyota" 2013 "XZF" & insert
      rover <- Car "Perseverance" 2021 "MARS" & insert
      corolla <- Car "Corolla Toyota" 2011 "P4E" & insert
      PersonCar providence venza & insert
      PersonCar nelly venza & insert
      PersonCar kayla rover & insert
      PersonCar nelly corolla & insert
      PersonStore providence costco & insert
      PersonStore nelly superC & insert
      PersonStore nelly maxi & insert
      PersonStore kayla mars & insert
      pure ()
