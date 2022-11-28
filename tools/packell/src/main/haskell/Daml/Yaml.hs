-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Daml.Yaml (
    T
  , Config(..)
  , damlConfigFile
  , readDamlConfig
  , writeDamlConfig
) where

import Control.Applicative
import Data.Yaml (FromJSON(parseJSON), (.:), Value(Object), decodeFileEither, ParseException, (.:?), encodeFile, ToJSON(toJSON, toEncoding), object, (.=), encode, defaultEncodeOptions)
import Data.Aeson (pairs)
import Data.Yaml.Pretty (encodePretty, defConfig, setConfCompare)
import Data.Function (on)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.ByteString.UTF8 (toString)

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

instance ToJSON Config where
  toJSON (Config sdkVersion name source version dependencies dataDependencies buildOptions) =
    object [
        "sdk-version" .= sdkVersion
      , "name" .= name
      , "source" .= source
      , "version" .= version
      , "dependencies" .= dependencies
      , "data-dependencies" .= dataDependencies
      , "build-options" .= buildOptions
      ]
  toEncoding (Config sdkVersion name source version dependencies dataDependencies buildOptions) =
    pairs $
      "sdk-version" .= sdkVersion
      <> "name" .= name
      <> "source" .= source
      <> "version" .= version
      <> "dependencies" .= dependencies
      <> "data-dependencies" .= dataDependencies
      <> "build-options" .= buildOptions

-- | Given a file path to a daml yaml config file, this will extract and return the contents.
readDamlConfig :: FilePath -> IO Config
readDamlConfig filePath = do
  damlConfig :: (Either ParseException Config) <- decodeFileEither filePath
  case damlConfig of
    Left exception -> error $ "Exception occured while reading daml yaml. filePath=" <> filePath <> ", exception=" <> show exception
    Right dc -> pure dc

-- | Writes a daml config file to the specified path.
writeDamlConfig :: FilePath -> Config -> IO ()
writeDamlConfig path = writeFile path . generateYaml

-- | Generate daml yaml config.
generateYaml :: Config -> String
generateYaml config =
  let
    fieldOrder = [
        "sdk-version"
      , "name"
      , "source"
      , "version"
      , "dependencies"
      , "data-dependencies"
      , "build-options"
      ]
    fieldIndex s = fromMaybe (length fieldOrder) $ s `elemIndex` fieldOrder
    encodeOptions = setConfCompare (compare `on` fieldIndex) defConfig
    yamlByteString = encodePretty encodeOptions config
  in
    indentList $ toString yamlByteString

-- | Add indentation when writing yaml lists.
indentList :: String -> String
indentList ('-':' ':xs)      = ' ':' ':'-':' ': indentList xs
indentList (x:xs)            = x : indentList xs
indentList []                = ""
