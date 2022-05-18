{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
import qualified Text.Feed.Import as Import (parseFeedFromFile)
import qualified Text.Feed.Query as Query (getFeedHome)
import qualified Text.Feed.Types as Types

import Data.Time.Clock  as C
import Data.Time.Calendar

import qualified Text.Pandoc as P
import qualified Data.Text.IO as TIO
import qualified Text.Pandoc.Shared as TS

hook :: Args -> IO ()
hook Help = putStrLn help
hook (New feed uri title)     = create feed title uri
hook (Add feed feedDestPath postPath post) = add feed feedDestPath postPath post
hook (AddExt feed feedDestPath postPath post) = addExt feed feedDestPath postPath post

today :: IO (Integer,Int,Int)
today = C.getCurrentTime >>= return . toGregorian . utctDay

todayText :: IO T.Text
todayText = do
  (year,month,day) <- today
  return.T.pack $ (show year)++"-"++(show month)++"-"++(show day)

outputFeed :: Atom.Feed -> FilePath -> IO ()
outputFeed feed path = do
  let mrenderedfeed = Export.textFeed $ feed
  case mrenderedfeed of
    Just renderedfeed -> do
      let rfeed = L.unpack renderedfeed      
      writeFile path rfeed
    Nothing -> putStrErr $ "Failed to create new feed: "++path

create :: FPath -> String -> URI -> IO ()
create feedPath title uri = do
  let path = filePath feedPath
  feed <- newfeed  
  outputFeed feed path
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

-- Retrieves the title, date, external URL and abstract from the markdown file post.
--                                                                        external
--                                                          title, date,  URL,      abstract     
getMeta :: FPath -> IO (Either P.PandocError (Either Error (String,String,Maybe URI,String)))
getMeta path = do
  let postPath = filePath path
  mkd <- (readFile postPath) >>= (return . T.pack)
  P.runIO $ do
    (P.Pandoc m b) <- P.readMarkdown readerOpts mkd
    let titleP = T.unpack $ TS.stringify $ P.docTitle m
    let dateP = T.unpack $ TS.stringify $ P.docDate m
    let eurl' = P.lookupMeta (T.pack "external-url") m
    let abstract' = P.lookupMeta (T.pack "abstract") m
    case (eurl',abstract')  of 
      (Just (P.MetaInlines eurl''), Just (P.MetaInlines abstract'')) -> do
        let urlStr = T.unpack $ TS.stringify $ eurl''
        let abstractStr = T.unpack $ TS.stringify $ abstract'' 
        case parseURI urlStr of
          Right eurl ->
            return $ Right (titleP, dateP, Just eurl, abstractStr)
          Left err -> return $ Left err
      (Just (P.MetaInlines eurl''), Nothing) -> do
        let urlStr = T.unpack $ TS.stringify $ eurl''
        case parseURI urlStr of
          Right eurl ->
            return $ Right (titleP, dateP, Just eurl, "")
          Left err -> return $ Left err
      (Nothing, Just (P.MetaInlines abstract'')) -> do
        let abstractStr = T.unpack $ TS.stringify $ abstract'' 
        return $ Right (titleP, dateP, Nothing, abstractStr)
      (Nothing, Nothing) -> do
        return $ Right (titleP, dateP, Nothing, "")

-- Builds the absoulte url to the post located at postPath.  The
-- returned URL is of the form:
--     freeId/mpostsPath/postpath
postURL :: URI -> Maybe URI -> FPath -> Maybe URI
postURL feedId mpostsPath postPath =
  case mpostsPath of
    Just postsPath ->
      do postFile <- parseRelURI postRel
         (feedId `appendRelURI` postsPath) >>= (\x -> appendRelURI x postFile)
    Nothing ->
      do postFile <- parseRelURI postRel
         appendRelURI feedId postFile
 where
   postRel :: FilePath
   postRel = filePath $ (replaceExt ".html" $ fileName postPath)

getFeedId :: Types.Feed -> Either Error URI
getFeedId (Types.AtomFeed (Atom.Feed id _ _ _ _ _ _ _ _ _ _ _ _ _ _)) = parseURI . T.unpack $ id
getFeedId feed = Left . Error $ "Failed to parse feed"

feedId :: FPath -> Types.Feed -> Either Error URI
feedId feedPath feed =
  case getFeedId feed of  
    Right id -> Right id
    Left _ -> Left . Error $ "Failed to retrieve url from feed: "++(show feedPath)

importFeed :: FPath -> IO (Either Error Types.Feed)
importFeed (filePath -> feedPath) = do
  mfeed <- Import.parseFeedFromFile feedPath
  return $ case mfeed of
    Just feed -> Right feed
    Nothing -> Left . Error $ "Failed to parse feed: "++(show feedPath)    

entry :: String -> String -> String -> String -> String -> Atom.Entry
entry id title date url abstract =
  (Atom.nullEntry
     (T.pack id)
     (Atom.TextString (T.pack title))
     (T.pack date))
  { Atom.entryAuthors = [Atom.nullPerson]
  , Atom.entryLinks = [Atom.nullLink (T.pack url)]
  , Atom.entryContent = Just (Atom.HTMLContent (T.pack abstract))
  }

updateFeed :: Types.Feed -> Atom.Entry -> IO (Maybe Atom.Feed)
updateFeed (Types.AtomFeed feed) entry = do
  date <- todayText
  return . Just $ feed { Atom.feedUpdated = date,
                         Atom.feedEntries = entry:(Atom.feedEntries feed) } 
updateFeed _ _ = return Nothing

add :: FPath -> FPath -> Maybe URI -> FPath -> IO ()
add feedPath feedDestPath postPath post = do    
  d <- getMeta post
  case d of
    Right (Right (t,d,Nothing,a)) -> do
      mfeed <- importFeed feedPath
      case mfeed of
        Right feed -> do
          let mid = feedId feedPath feed
          case mid of
            Right id -> do
              case postURL id postPath post of
                Just url -> do
                  let newEntry = entry (uriToString id) t d (uriToString url) a
                  mupFeed <- updateFeed feed newEntry
                  case mupFeed of
                    Just updatedFeed -> do                      
                      outputFeed updatedFeed (filePath feedDestPath)
                    Nothing -> putStrErr $ "Failed to add "++(show postPath)++" to feed "++(show feed)
                Nothing -> putStrErr $ "Failed to retrieve url from feed: "++(show feed)
            Left (Error err) -> putStrErr err
        Left (Error err) -> putStrErr err
    Right (Left err) -> putStrErr $ "Failed to retrieve metadata from post: "++(errorToStr err)  
    Left e -> putStrErr $ "Failed to retrieve metadata from post: "++(show e)  

addExt :: FPath -> FPath -> Maybe URI -> FPath -> IO ()
addExt feedPath feedDestPath postPath post = do    
  d <- getMeta post
  case d of
    Right (Right (t,a,(Just eurl),d)) -> do
      mfeed <- importFeed feedPath
      case mfeed of
        Right feed -> do
          let mid = feedId feedPath feed
          case mid of
            Right id -> do
              let newEntry = entry (uriToString id) t d (uriToString eurl) a
              mupFeed <- updateFeed feed newEntry
              case mupFeed of
                Just updatedFeed ->
                  outputFeed updatedFeed (filePath feedDestPath)
                Nothing -> putStrErr $ "Failed to add "++(show postPath)++" to feed "++(show feed)                
            Left (Error err) -> putStrErr err
        Left (Error err) -> putStrErr err
    Right (Right (t,a,Nothing,d)) -> putStrErr $ "Failed to retrieve metadata from post: "++(show post)  
    Right (Left err) -> putStrErr $ "Failed to retrieve metadata from post: "++(errorToStr err)  
    Left e -> putStrErr $ "Failed to retrieve metadata from post: "++(show e)  
