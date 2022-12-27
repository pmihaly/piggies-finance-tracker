module Main (main) where

import Application.CLI.InputFile (InputFile (..), toState)
import Application.Shared.PlayEvents (playEvents)
import Control.Category ((>>>))
import Data.Yaml (ParseException, decodeFileEither, encode)
import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.ByteString as BS

main :: IO ()
main =
  getArgs
    >>= (head >>> decodeFileEither :: [String] -> IO (Either ParseException InputFile))
    >>= either (show >>> die) (\file -> pure $ playEvents (toState file) (events file))
    >>= either (show >>> die) (encode >>> BS.putStr)
