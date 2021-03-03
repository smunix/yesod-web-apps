{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- |
module Chap7 where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text
import qualified Data.Text as T
import Yesod

data Sub where
  Sub :: Sub

data App where
  App :: App

instance Yesod App

newtype I where
  I :: Int -> I
  deriving (Eq, Show, Read)

instance PathPiece I where
  fromPathPiece s = case s & T.unpack & reads of
    (i, "") : _
      | i < 1 -> Nothing
      | otherwise -> i & I & Just
    [] -> Nothing
  toPathPiece (I i) = i & show & T.pack

data Page where
  Page :: Text -> Text -> [Text] -> Page

instance PathMultiPiece Page where
  fromPathMultiPiece (x : y : z) = Page x y z & Just
  fromPathMultiPiece _ = Nothing
  toPathMultiPiece (Page x y z) = x : y : z

data Y where
  Y :: Int -> Y
  SmallY :: Int -> Y
  BigY :: Int -> Y
  deriving (Eq, Show, Read)

data Y' r where
  Y' :: Int -> Y' r
  SmallY' :: Int -> (Int -> r) -> Y' r
  BigY' :: Int -> Y' r

instance PathPiece Y where
  fromPathPiece s = case s & T.unpack & reads of
    (y, "") : _
      | y < 1960 -> SmallY y & pure
      | 2050 < y -> BigY y & pure
      | otherwise -> y & Y & Just
    [] -> Nothing
  toPathPiece (Y y) = y & show & T.pack

newtype M where
  M :: Int -> M
  deriving (Eq, Show, Read)

instance PathPiece M where
  fromPathPiece s = case s & T.unpack & reads of
    (y, "") : _
      | y < 1 -> Nothing
      | 12 < y -> Nothing
      | otherwise -> y & M & Just
    [] -> Nothing
  toPathPiece (M y) = y & show & T.pack

newtype D where
  D :: Int -> D
  deriving (Eq, Show, Read)

instance PathPiece D where
  fromPathPiece s = case s & T.unpack & reads of
    (y, "") : _
      | y < 1 -> Nothing
      | 31 < y -> Nothing
      | otherwise -> y & D & Just
    [] -> Nothing
  toPathPiece (D y) = y & show & T.pack

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/page/faq FaqR
/person/#String PersonR GET
/blog/#I BlogR GET
/year/#Y/month/#M/day/#D DateR
/link1 Link1R GET
/link2 Link2R GET
/link3 Link3R GET
/link4 Link4R GET

-- /sub SubR Sub sub
|]

getHomeR :: Handler Html
getHomeR = defaultLayout do
  setTitle "redirects"
  [whamlet|
     <p>
       <a href=@{Link1R}>Click to start the redirect loop!
  |]

getLink1R, getLink2R, getLink3R, getLink4R :: Handler Html
getLink1R = redirect Link2R
getLink2R = redirect (Link3R, [("foo", "bar")]) -- /link3?foo=bar
getLink3R = Link4R :#: ("baz" :: Text) & redirect
getLink4R =
  defaultLayout
    [whamlet|
      <p>
        You made it until here!
    |]

handleFaqR :: Handler Html
handleFaqR = error "nyi"

getPersonR :: [] Char -> Handler Html
getPersonR = error "nyi"

getBlogR :: I -> Handler Html
getBlogR = error "nyi"

handleDateR :: Y -> M -> D -> Handler Text
handleDateR (SmallY y) _ _ = do
  let !err = (y & show & T.pack) <> " < 1960"
  $logError err
  err & pure & invalidArgs
handleDateR (BigY y) _ _ = do
  let !err = "2050 < " <> (y & show & T.pack)
  $logError err
  err & pure & invalidArgs
handleDateR y m d = [show y, show m, show d] <&> T.pack & T.intercalate " " & pure
