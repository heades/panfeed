{-# LANGUAGE OverloadedStrings #-}
module Lib (
  module Utils,
  hook
) where

import Utils

import Data.Text as T
import Data.Text.Lazy as L
import Data.XML.Types as XML
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export (textFeed)

import Data.Time.Clock
import Data.Time.Calendar

hook :: Args -> IO ()
hook Help = putStrLn help
hook (New feed uri title) = create feed title uri
hook (Add feed post) = putStrErr "TODO"

today :: IO (Integer,Int,Int)
today = getCurrentTime >>= return . toGregorian . utctDay

todayText :: IO T.Text
todayText = do
  (year,month,day) <- today
  return.T.pack $ (show year)++"-"++(show month)++"-"++(show day)

create :: SomeBase File -> String -> URI -> IO ()
create feedPath title uri = do
  let path = fromSomeFile feedPath
  feed <- newfeed  
  let mrenderedfeed = Export.textFeed $ feed
  case mrenderedfeed of
    Just renderedfeed -> do
      let rfeed = L.unpack renderedfeed      
      writeFile path rfeed
    Nothing -> error $ "Failed to create new feed: "++path++"\n"
 where
   newfeed :: IO Atom.Feed
   newfeed = do     
     date <- todayText
     return $ Atom.nullFeed
       (T.pack.show $ uri)
       (Atom.TextString (T.pack title))
       date
