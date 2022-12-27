{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Application.CLI.InputOutputFile (InputOutputFile (..), fromStateWithEvents, toState)
import Application.Shared.PlayEvents (playEvents)
import Control.Category ((>>>))
import Data.ByteString qualified as BS
import Data.Yaml (ParseException, decodeFileEither, encode)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main =
  getArgs
    >>= (head >>> decodeFileEither :: [String] -> IO (Either ParseException InputOutputFile))
    >>= either (show >>> die) (\file -> pure $ playEvents (toState file) (events file))
    >>= either (show >>> die) (fromStateWithEvents [] >>> encode >>> BS.putStr)
