module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  (o, n) <- panfeedOpts args
  case checkInputs o n of
    Right arg -> hook arg
    Left (Error err) -> putStrErr $ err ++ "\n\n"++help
