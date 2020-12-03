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

import Data.Time.Clock  as C
import Data.Time.Calendar

import qualified Text.Pandoc as P
import qualified Data.Text.IO as TIO
import qualified Text.Pandoc.Shared as TS

hook :: Args -> IO ()
hook Help = putStrLn help
hook (New feed uri title) = create feed title uri
hook (Add feed post) = add feed post

today :: IO (Integer,Int,Int)
today = C.getCurrentTime >>= return . toGregorian . utctDay

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
    Nothing -> putStrErr $ "Failed to create new feed: "++path
 where
   newfeed :: IO Atom.Feed
   newfeed = do     
     date <- todayText
     return $ Atom.nullFeed
       (T.pack.show $ uri)
       (Atom.TextString (T.pack title))
       date

readerOpts :: P.ReaderOptions
readerOpts = P.ReaderOptions P.pandocExtensions True 1 1 P.def P.def "" P.RejectChanges True

getMeta :: SomeBase File -> IO (Either P.PandocError (String,String))
getMeta post = do
  let postPath = fromSomeFile post
  mkd <- (readFile postPath) >>= (return . T.pack)
  P.runIO $ do
    (P.Pandoc m b) <- P.readMarkdown readerOpts mkd
    let titleP = show $ TS.stringify $ P.docTitle m
    let dateP = show $ TS.stringify $ P.docDate m
    return (titleP,dateP)

entry :: String -> String -> URI -> Atom.Entry
entry title date url = undefined

feedUrl :: SomeBase File -> URI
feedUrl = undefined

add :: SomeBase File -> SomeBase File -> IO ()
add feed post = do  
  let feedPath = fromSomeFile feed
  d <- getMeta post
  case d of
    Right (t,d) -> do
      let url = feedUrl feed
      let newEntry = entry t d url
      return ()
    Left e -> return ()
  return ()

