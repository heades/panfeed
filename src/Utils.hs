{-# LANGUAGE ViewPatterns #-}
module Utils (
  module System.IO,
  putStrErr,
  module Network.URI,
  module Path.Posix,  
  panfeedOpts,
  checkInputs,
  help,
  Error(..),
  Args(..)
) where

import System.IO
import Network.URI
import Path.Posix
import System.Console.GetOpt

newtype Error = Error String
  deriving (Show,Eq)

putStrErr :: String -> IO ()
putStrErr = hPutStrLn stderr

data Options = OHelp
             | ONew
             | OURL (Either Error URI)
             | OTitle String
             | OAdd (Either Error (SomeBase File))
  deriving (Show,Eq)

cmdErr :: String -> Either Error a
cmdErr str = Left (Error str)

options :: [OptDescr Options]
options = [ Option ['h'] ["help"]  (NoArg OHelp)                           "the help message.",
            Option ['n'] ["new"]   (NoArg ONew)                            "create a new empty FEED.rss (requires --url and --title).",
            Option ['u'] ["url"]   (ReqArg parseURL  "URL")                "specify the URL to a new feeds website.",
            Option ['t'] ["title"] (ReqArg OTitle     "TITLE")             "specify the TITLE to a new feed.",
            Option ['a'] ["add"]   (ReqArg (parseAdd ".md")  "POST.md")       "add the markdown file POST.md to FEED.rss."]

parseURL :: String -> Options
parseURL url = OURL $ 
  case (parseURI url) of
    Just uri -> Right $ uri
    Nothing -> Left . Error $ "invalid url"

fileExt :: SomeBase File -> Maybe String
fileExt (Abs p) = fileExtension p
fileExt (Rel p) = fileExtension p

parseFilePath :: String -> String -> Either Error (SomeBase File)
parseFilePath ext path = do
  case mpath of
    Just p ->
      case (fileExt p) of
        Just e -> 
          if e == ext
          then Right p
          else Left . Error $ "invalid file extension "++e
        Nothing -> Left . Error $ "invalid file path "++(show p)
 where
    mpath = parseSomeFile path :: Maybe (SomeBase File)

parseAdd :: String -> String -> Options
parseAdd ext path = OAdd $ parseFilePath ext path

help :: String
help = usageInfo header options
 where
    header = "panfeed is a Pandoc-based websites RSS feed manager.\n\npandoc [OPTIONS] FEED.rss\n\nAvailable OPTIONS:"

panfeedOpts :: [String] -> IO ([Options], [String])
panfeedOpts argv =
  case getOpt RequireOrder options argv of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError . userError $ concat errs ++ help

data Args = Help
          | New (SomeBase File) URI String
          | Add (SomeBase File) (SomeBase File)
 deriving Show

parseNew :: String -> Either Error URI -> String -> Either Error Args
parseNew feed (Right url) title =
  case mfeed of
    Right feedpath -> Right $ New feedpath url title
    Left err -> Left err
 where
   mfeed = parseFilePath ".rss" feed    
parseNew feed (Left err) title = Left err
   
newOpt :: String -> [Options] -> Either Error Args
newOpt feed (ONew :  OURL url : OTitle title : []) = parseNew feed url title
newOpt feed (OURL url : ONew : OTitle title : [])  = parseNew feed url title
newOpt feed (OURL url : OTitle title : ONew : [])  = parseNew feed url title
newOpt feed (OTitle title : OURL url : ONew : [])  = parseNew feed url title
newOpt feed (OTitle title : ONew : OURL url : [])  = parseNew feed url title
newOpt feed (ONew : OTitle title : OURL url : [])  = parseNew feed url title
newOpt _ _ = Left . Error $ "--new requires both --url and --title see --help."

addOpt :: String -> Either Error (SomeBase File) -> Either Error Args
addOpt feed (Right post) =
  case mfeed of
    Right feedPath -> Right $ Add feedPath post
    Left err -> Left err
 where
   mfeed = parseFilePath ".rss" feed
addOpt feed (Left err) = Left err

checkInputs :: [Options] -> [String] -> Either Error Args
checkInputs [OHelp] [] = Right Help
checkInputs o [feed] | ONew `elem` o = newOpt feed o
checkInputs [OAdd post] [feed] = addOpt feed post
checkInputs _ _ = Left . Error $ "Invalid options see --help."
