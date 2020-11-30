{-# LANGUAGE OverloadedStrings #-}
module Lib () where

import Utils

import Data.Text
import Data.XML.Types as XML
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export (textFeed)

import Data.Time.Clock
import Data.Time.Calendar

today :: IO (Integer,Int,Int)
today = getCurrentTime >>= return . toGregorian . utctDay

todayText :: IO Text
todayText = do
  (year,month,day) <- today
  return.pack $ (show year)++"-"++(show month)++"-"++(show day)

create :: Feed -> Title -> URI -> IO ()
create (Feed feed) (Title title) uri = do  
  return ()
 where
   newfeed = do     
     date <- todayText
     return $ Atom.nullFeed
       (pack.show $ uri)
       (Atom.TextString (pack title))
       date
