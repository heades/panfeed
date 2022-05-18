{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Utils (
  module System.IO,
  module System.FilePath,
  FPath(..),
  filePath,
  putStrErr,
  U.URI(..),
  panfeedOpts,
  checkInputs,
  help,
  Error(..),
  errorToStr,
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
import System.FilePath
import System.Console.GetOpt

data FPath = Rel FilePath | Abs FilePath
  deriving (Show, Eq)

mapFPath :: (FilePath -> FilePath) -> FPath -> FPath
mapFPath f (Rel p) = Rel . f $ p
mapFPath f (Abs p) = Abs . f $ p

foldFPath :: (FilePath -> a) -> FPath -> a
foldFPath f (Rel p) = f p
foldFPath f (Abs p) = f p

filePath :: FPath -> FilePath
filePath = foldFPath id

newtype Error = Error String
  deriving (Show,Eq)

errorToStr :: Error -> String
errorToStr (Error s) = s

putStrErr :: String -> IO ()
putStrErr err = hPutStrLn stderr $ err ++"\n\n"++help

data Options = OHelp
             | ONew
             | OURL (Either Error U.URI)
             | OTitle String
             | OAdd (Either Error FPath)
             | OAddExt (Either Error FPath)
             | OPostPath (Either Error U.URI)
             | ODestPath (Either Error FPath)
  deriving (Show,Eq)

cmdErr :: String -> Either Error a
cmdErr str = Left (Error str)

options :: [OptDescr Options]
options = [ Option ['h'] ["help"]      (NoArg OHelp)                          "the help message.",
            Option ['n'] ["new"]       (NoArg ONew)                           "create a new empty FEED.xml (requires --url and --title).",
            Option []    ["url"]       (ReqArg parseURL         "URL")        "specify the URL to a new feeds website.",
            Option []    ["title"]     (ReqArg OTitle           "TITLE")      "specify the TITLE to a new feed.",
            Option ['a'] ["add"]       (ReqArg (parseAdd ".md") "POST.md")    "add the markdown file POST.md to FEED.xml.",
            Option [] ["add-external"] (ReqArg (parseAddExt ".md") "POST.md")    "add the markdown file POST.md as an externally linked blog post to FEED.xml.",
            Option []    ["post-path"] (ReqArg parsePostPath    "PATH")       "specify the releative path of the URL to where the HTML posts live (requires --add or --add-external).",
            Option []    ["feed-dest"] (ReqArg parseDestPath    "PATH")       "the PATH to the newly created feed (requires --add)." ]

feedExt :: String
feedExt = ".xml"

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

fileExt :: FPath -> String
fileExt = foldFPath takeExtension

fileName :: FPath -> FPath
fileName = mapFPath takeBaseName

replaceExt :: String -> FPath -> FPath
replaceExt ext = mapFPath (\x -> replaceExtension x ext)

parseFilePath :: String -> Either Error FPath
parseFilePath path = if (isValid path)
                     then Right $ if (isAbsolute path)
                                  then Abs $ path
                                  else Rel $ path                      
                      else Left . Error $ "invalid file path "++path

checkExt :: String -> FPath -> Either Error FPath
checkExt ext p | foldFPath (\x -> takeExtension x == ext) p = Right p
checkExt ext p | otherwise = Left . Error $ "invalid file path extension "++ext

parseAdd :: String -> String -> Options
parseAdd ext path = OAdd $
  case fp of
    Right p -> checkExt ext p
    _ -> fp
 where
   fp = parseFilePath path

parseAddExt :: String -> String -> Options
parseAddExt ext path = OAddExt $
  case fp of
    Right p -> checkExt ext p
    _ -> fp
 where
   fp = parseFilePath path   

parsePostPath :: String -> Options
parsePostPath postPathStr = OPostPath $
  case parseRelURI postPathStr of
    Just postPath -> Right postPath
    Nothing -> Left . Error $ "invalied post path: "++postPathStr

parseDestPath :: String -> Options
parseDestPath destPathStr = ODestPath $ parseFilePath destPathStr

help :: String
help = usageInfo header options
 where
    header = "panfeed is a Pandoc-based websites RSS feed manager.\n\npandoc [OPTIONS] FEED.xml\n\nAvailable OPTIONS:"

panfeedOpts :: [String] -> IO ([Options], [String])
panfeedOpts argv =
  case getOpt RequireOrder options argv of
    (o,n,[]) -> return (o,n)
    (_,_,errs) -> ioError . userError $ concat errs ++ help

data Args = Help
          | New FPath U.URI String
          -- Add feedPath feedDestPath postURI  mdPath
          | Add FPath FPath (Maybe U.URI) FPath
          | AddExt FPath FPath (Maybe U.URI) FPath
 deriving Show

parseNew :: String -> Either Error U.URI -> String -> Either Error Args
parseNew feed (Right url) title =
  case mfeed of
    Right feedpath ->
      case checkExt feedExt feedpath of
        Right _ -> Right $ New feedpath url title
        Left err -> Left err
    Left err -> Left err
 where
   mfeed = parseFilePath feed    
parseNew feed (Left err) title = Left err
   
newOpt :: String -> [Options] -> Either Error Args
newOpt feed (ONew :  OURL url : OTitle title : []) = parseNew feed url title
newOpt feed (OURL url : ONew : OTitle title : [])  = parseNew feed url title
newOpt feed (OURL url : OTitle title : ONew : [])  = parseNew feed url title
newOpt feed (OTitle title : OURL url : ONew : [])  = parseNew feed url title
newOpt feed (OTitle title : ONew : OURL url : [])  = parseNew feed url title
newOpt feed (ONew : OTitle title : OURL url : [])  = parseNew feed url title
newOpt _ _ = Left . Error $ "--new requires both --url and --title see --help."

parseAddPostPath :: FilePath -> Maybe (Either Error FPath) -> Either Error FPath -> Either Error U.URI -> Either Error Args
parseAddPostPath feed destPath (Right post) (Right path) =
  case (parseFilePath feed, destPath) of
    (Right feedPath, Just (Right feedDestPath)) ->
      case (checkExt feedExt feedPath, checkExt feedExt feedDestPath) of
        (Right _, Right _) -> Right $ Add feedPath feedDestPath (Just path) post
        (Right _, Left err) -> Left err
        (Left err, Right _) -> Left err
        (Left (Error err1), Left (Error err2)) -> Left . Error $ err1 ++ "\n\n" ++ err2
    (Right feedPath, Nothing) ->
      case (checkExt feedExt feedPath) of
        (Right _) -> Right $ Add feedPath feedPath (Just path) post
        (Left err) -> Left err
    (Left err, Nothing) -> Left err
    (Right _, Just (Left err)) -> Left err
    (Left (Error err1), Just (Left (Error err2))) -> Left . Error $ err1 ++ "\n\n" ++ err2
parseAddPostPath feed destPath (Left (Error err1)) (Left (Error err2)) = Left . Error $ err1 ++ "\n\n" ++ err2
parseAddPostPath feed destPath (Left err) _ = Left err
parseAddPostPath feed destPath _ (Left err) = Left err

parseAddExtPostPath :: FilePath -> Maybe (Either Error FPath) -> Either Error FPath -> Either Error U.URI -> Either Error Args
parseAddExtPostPath feed destPath (Right post) (Right path) =
  case (parseFilePath feed, destPath) of
    (Right feedPath, Just (Right feedDestPath)) ->
      case (checkExt feedExt feedPath, checkExt feedExt feedDestPath) of
        (Right _, Right _) -> Right $ AddExt feedPath feedDestPath (Just path) post
        (Right _, Left err) -> Left err
        (Left err, Right _) -> Left err
        (Left (Error err1), Left (Error err2)) -> Left . Error $ err1 ++ "\n\n" ++ err2
    (Right feedPath, Nothing) ->
      case (checkExt feedExt feedPath) of
        (Right _) -> Right $ AddExt feedPath feedPath (Just path) post
        (Left err) -> Left err
    (Left err, Nothing) -> Left err
    (Right _, Just (Left err)) -> Left err
    (Left (Error err1), Just (Left (Error err2))) -> Left . Error $ err1 ++ "\n\n" ++ err2
parseAddExtPostPath feed destPath (Left (Error err1)) (Left (Error err2)) = Left . Error $ err1 ++ "\n\n" ++ err2
parseAddExtPostPath feed destPath (Left err) _ = Left err
parseAddExtPostPath feed destPath _ (Left err) = Left err

addOpt :: FilePath -> [Options] -> Either Error Args
addOpt feed [OAdd mpost] = parseAddPostPath feed Nothing mpost (Right U.nullURI)
addOpt feed (OAdd mpost:OPostPath mpath:[]) = parseAddPostPath feed Nothing mpost mpath
addOpt feed (OPostPath mpath:OAdd mpost:[]) = parseAddPostPath feed Nothing mpost mpath
addOpt feed (OAdd mpost:OPostPath mpath:ODestPath destPath:[]) = parseAddPostPath feed (Just destPath) mpost mpath
addOpt feed (OAdd mpost:ODestPath destPath:OPostPath mpath:[]) = parseAddPostPath feed (Just destPath) mpost mpath
addOpt feed (ODestPath destPath:OAdd mpost:OPostPath mpath:[]) = parseAddPostPath feed (Just destPath) mpost mpath
addOpt feed (ODestPath destPath:OPostPath mpath:OAdd mpost:[]) = parseAddPostPath feed (Just destPath) mpost mpath
addOpt feed (OPostPath mpath:ODestPath destPath:OAdd mpost:[]) = parseAddPostPath feed (Just destPath) mpost mpath
addOpt feed (OPostPath mpath:OAdd mpost:ODestPath destPath:[]) = parseAddPostPath feed (Just destPath) mpost mpath
addOpt _ _ = Left . Error $ "in valid --add.\n\n"++help

addExtOpt :: FilePath -> [Options] -> Either Error Args
addExtOpt feed [OAddExt mpost] = parseAddExtPostPath feed Nothing mpost (Right U.nullURI)
addExtOpt feed (OAddExt mpost:OPostPath mpath:[]) = parseAddExtPostPath feed Nothing mpost mpath
addExtOpt feed (OPostPath mpath:OAddExt mpost:[]) = parseAddExtPostPath feed Nothing mpost mpath
addExtOpt feed (OAddExt mpost:OPostPath mpath:ODestPath destPath:[]) = parseAddExtPostPath feed (Just destPath) mpost mpath
addExtOpt feed (OAddExt mpost:ODestPath destPath:OPostPath mpath:[]) = parseAddExtPostPath feed (Just destPath) mpost mpath
addExtOpt feed (ODestPath destPath:OAddExt mpost:OPostPath mpath:[]) = parseAddExtPostPath feed (Just destPath) mpost mpath
addExtOpt feed (ODestPath destPath:OPostPath mpath:OAddExt mpost:[]) = parseAddExtPostPath feed (Just destPath) mpost mpath
addExtOpt feed (OPostPath mpath:ODestPath destPath:OAddExt mpost:[]) = parseAddExtPostPath feed (Just destPath) mpost mpath
addExtOpt feed (OPostPath mpath:OAddExt mpost:ODestPath destPath:[]) = parseAddExtPostPath feed (Just destPath) mpost mpath
addExtOpt _ _ = Left . Error $ "in valid --add.\n\n"++help

isAdd :: Options -> Bool
isAdd (OAdd _) = True
isAdd _ = False

isAddExt :: Options -> Bool
isAddExt (OAddExt _) = True
isAddExt _ = False

hasAdd :: [Options] -> Bool
hasAdd = foldr (\o r -> if isAdd o then True else r) False

hasAddExt :: [Options] -> Bool
hasAddExt = foldr (\o r -> if isAddExt o then True else r) False

checkInputs :: [Options] -> [String] -> Either Error Args
checkInputs [OHelp] [] = Right Help
checkInputs o [feed] | ONew `elem` o = newOpt feed o
checkInputs o [feed] | hasAdd o = addOpt feed o
checkInputs o [feed] | hasAddExt o = addExtOpt feed o
checkInputs o feed = Left . Error . show $ feed
