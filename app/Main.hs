{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Application.CLI.InputOutputFile (InputOutputFile (..), fromStateWithEvents, toState)
import Application.Shared.PlayEvents (playEvents)
import Data.ByteString qualified as BS
import Data.Yaml (decodeFileEither, encode)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  filePath <- head <$> getArgs
  fileEither <- decodeFileEither filePath
  case fileEither of
    Left err -> die $ show err
    Right file -> do
      let state = toState file
      let events' = events file
      case playEvents state events' of
        Left err -> die $ show err
        Right newState -> BS.putStr $ encode $ fromStateWithEvents events' newState
