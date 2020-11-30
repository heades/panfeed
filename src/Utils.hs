{-# LANGUAGE ViewPatterns #-}
module Utils () where

import Network.URI
import Path.Posix

newtype Title = Title String
newtype Feed  = Feed (SomeBase File)
newtype Post  = Post (SomeBase File)
newtype Error = Error String

data Args = Help | New Feed Title URI | Add Feed Post

instance (Show Title) where
  show (Title s) = s  

instance (Show Error) where
  show (Error s) = s

showArgs :: Args -> String
showArgs Help = "--help"
showArgs (New (Feed feed) (Title title) uri) = unwords ["--new",(show feed),title,uriStr]
  where
    uriStr = (uriToString id uri) ""
showArgs (Add (Feed feed) (Post post)) = unwords ["--add",(show feed),(show post)]

instance (Show Args) where
  show = showArgs

cmdErr :: String -> Either Error a
cmdErr str = Left (Error str)

help :: String
help = "panfeed is a Pandoc-based websites RSS feed manager.\n\n"++
       "Availiable options:\n" ++
       "--help               : This help message.\n" ++        
       "--new feed title url : create a new empty feed with metadata title and url.\n" ++
       "--add feed post      : add post as a new entry to feed.\n"

fileExt :: SomeBase File -> Maybe String
fileExt (Abs p) = fileExtension p
fileExt (Rel p) = fileExtension p

parseArgsError :: [String] -> String -> Either Error Args
parseArgsError args msg = cmdErr $ (unwords args) ++ msg++"\n\n" ++ help

parseArgs :: String -> Either Error Args
parseArgs (words -> ["--help"]) = Right Help
parseArgs (words -> args@("--new":feed:title:url:[])) =
  case mfeed of
    Just feedPath ->
      case (fileExt feedPath) of
        (Just ".rss") ->
          case (parseURI url) of
            Just uri -> Right $ New (Feed feedPath) (Title title) uri
            Nothing -> parseArgsError args " : invalid url provided."
        Nothing -> parseArgsError args " : feed file extension must be '.rss'."
    Nothing -> parseArgsError args " : invalid path to the feed provided."
 where  
   mfeed = parseSomeFile feed :: Maybe (SomeBase File)
parseArgs (words -> args@("--add":feed:post:[])) =
  case (mfeed,mpost) of
    (Just feedPath, Just postPath) ->
      case (fileExt feedPath, fileExt postPath) of
        (Just ".rss", Just ".md") -> Right $ Add (Feed feedPath) (Post postPath)
        (Nothing, Just ".md") -> parseArgsError args " : feed file extension must be '.rss'."
        (Just ".rss", Nothing) -> parseArgsError args " : post file extension must be '.md'."
        (_, _) -> parseArgsError args " : feed and post file extensions must be '.rss' and '.md' respectively."
    (Nothing, Just _) -> parseArgsError args " : invalid path to the feed provided."
    (Just _, Nothing) -> parseArgsError args " : invalid path to the post provided."
    (Nothing,Nothing) -> parseArgsError args " : invalid path to the both feed and post provided."   
  where
    mfeed = parseSomeFile feed :: Maybe (SomeBase File)
    mpost = parseSomeFile post :: Maybe (SomeBase File)
parseArgs (words -> args) = parseArgsError args " : not a valid command."
