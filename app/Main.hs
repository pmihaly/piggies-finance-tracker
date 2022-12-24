module Main (main) where

import Application.CLI.InputFile (InputFile)
import Control.Category ((>>>))
import Data.Yaml (ParseException, decodeFileEither)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main =
  getArgs
    >>= (head >>> decodeFileEither :: [String] -> IO (Either ParseException InputFile))
    >>= either (show >>> die) print
