{-# LANGUAGE ViewPatterns #-}
module Utils (
  module System.IO,
  putStrErr,
  U.URI(..),
  module Path.Posix,
  panfeedOpts,
  checkInputs,
  help,
  Error(..),
  Args(..),
  parseURI,
  parseRelURI,
  appendRelURI,
  fileName,
  replaceExt,
  uriToString,
) where

import System.IO
import qualified Network.URI as U
import Path.Posix
import System.Console.GetOpt

newtype Error = Error String
  deriving (Show,Eq)

putStrErr :: String -> IO ()
putStrErr err = hPutStrLn stderr $ err ++"\n\n"++help

data Options = OHelp
             | ONew
             | OURL (Either Error U.URI)
             | OTitle String
             | OAdd (Either Error (SomeBase File))
             | OPostPath (Either Error U.URI)
  deriving (Show,Eq)

cmdErr :: String -> Either Error a
cmdErr str = Left (Error str)

options :: [OptDescr Options]
options = [ Option ['h'] ["help"]      (NoArg OHelp)                          "the help message.",
            Option ['n'] ["new"]       (NoArg ONew)                           "create a new empty FEED.rss (requires --url and --title).",
            Option ['u'] ["url"]       (ReqArg parseURL         "URL")        "specify the URL to a new feeds website.",
            Option ['t'] ["title"]     (ReqArg OTitle           "TITLE")      "specify the TITLE to a new feed.",
            Option ['a'] ["add"]       (ReqArg (parseAdd ".md") "POST.md")    "add the markdown file POST.md to FEED.rss.",
            Option []    ["post-path"] (ReqArg parsePostPath    "PATH")       "specify the releative path of the URL to where the HTML posts live (requires --add)." ]

uriToString :: U.URI -> String
uriToString uri = U.uriToString id uri ""

appendRelURI :: U.URI -> U.URI -> Maybe U.URI
appendRelURI abs rel = U.parseURI uri
 where
   relStr = U.uriToString id rel ""
   uri = U.uriToString id abs $ "/" ++ relStr

parseRelURI :: String -> Maybe U.URI
parseRelURI url = U.parseRelativeReference url

parseURI :: String -> Either Error U.URI
parseURI url = case (U.parseURI url) of
    Just uri -> Right $ uri
    Nothing -> Left . Error $ "invalid url"

parseURL :: String -> Options
parseURL = OURL . parseURI

fileExt :: SomeBase File -> Maybe String
fileExt (Abs p) = fileExtension p
fileExt (Rel p) = fileExtension p

fileName :: SomeBase File -> Path Rel File
fileName (Abs p) = filename p
fileName (Rel p) = filename p

replaceExt :: String -> Path b File -> Maybe (Path b File)
replaceExt ext p = replaceExtension ext p

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

parsePostPath :: String -> Options
parsePostPath postPathStr = OPostPath $
  case parseRelURI postPathStr of
    Just postPath -> Right postPath
    Nothing -> Left . Error $ "invalied post path: "++postPathStr

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
          | New (SomeBase File) U.URI String
          | Add (SomeBase File) (Maybe U.URI) (SomeBase File)
 deriving Show

parseNew :: String -> Either Error U.URI -> String -> Either Error Args
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

parseAddPostPath :: String -> Either Error (SomeBase File) -> Either Error U.URI -> Either Error Args
parseAddPostPath feed (Right post) (Right path) =
  case parseFilePath ".rss" feed of
    Right feedPath -> Right $ Add feedPath (Just path) post
    Left err       -> Left err
parseAddPostPath feed (Left (Error err1)) (Left (Error err2)) = Left . Error $ err1 ++ "\n\n" ++ err2
parseAddPostPath feed (Left err) _ = Left err
parseAddPostPath feed _ (Left err) = Left err

addOpt :: String -> [Options] -> Either Error Args
addOpt feed [OAdd mpost] =
  case mpost of
    (Right post) ->
      case parseFilePath ".rss" feed of
        Right feedPath -> Right $ Add feedPath Nothing post
        Left err -> Left err
    (Left err) -> Left err
addOpt feed (OAdd mpost:OPostPath mpath:[]) = parseAddPostPath feed mpost mpath
addOpt feed (OPostPath mpath:OAdd mpost:[]) = parseAddPostPath feed mpost mpath

isAdd :: Options -> Bool
isAdd (OAdd _) = True
isAdd _ = False

hasAdd :: [Options] -> Bool
hasAdd = foldr (\o r -> if isAdd o then True else r) False

checkInputs :: [Options] -> [String] -> Either Error Args
checkInputs [OHelp] [] = Right Help
checkInputs o [feed] | ONew `elem` o = newOpt feed o
checkInputs o [feed] | hasAdd o = addOpt feed o
checkInputs _ _ = Left . Error $ "Invalid options see --help."
