{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Daml.Yaml where

import Control.Applicative
import Data.Yaml (FromJSON(parseJSON), (.:), Value(Object), decodeFileEither, ParseException, (.:?))

type T = Config

-- | Name of a daml config file on the filesystem.
damlConfigFile :: String = "daml.yaml"

-- | Represents a Daml config file
data Config = Config {
    sdkVersion :: String
  , name :: String
  , source :: String
  , version :: String
  , dependencies :: [String]
  , dataDependencies :: Maybe [String]
  , buildOptions :: Maybe [String]
  } deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
    v .:  "sdk-version" <*>
    v .:  "name" <*>
    v .:  "source" <*>
    v .:  "version" <*>
    v .:  "dependencies" <*>
    v .:? "data-dependencies" <*>
    v .:? "build-options"
  parseJSON _ = error "Cannot parse daml config from YAML"

-- Given a file path to a daml yaml config file, this will extract and return the contents
getDamlConfig :: FilePath -> IO Config
getDamlConfig filePath = do
  damlConfig :: (Either ParseException Config) <- decodeFileEither filePath
  case damlConfig of
    Left exception -> error (show exception)
    Right dc -> pure dc
