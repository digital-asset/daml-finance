-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Daml.Version (
    increment
  , updateVersion
) where

import qualified Daml.Package as Daml (Package(..))
import Daml.Package (Package(damlConfig, packageConfig))
import qualified Data.Text as T (pack)
import Daml.Types (UpdatedConfig(..))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import Package.Yaml (IncrementVersion(SNAPSHOT, PATCH, MINOR, MAJOR))
import qualified Package.Yaml as Package (Config(..), getLocalName, incrementVersion, Local (path))
import qualified Git.Commands as Git (tagExists)
import qualified Daml.Yaml as Daml (Config, version, damlConfigFile, writeDamlConfig)
import Colourista (cyanMessage, successMessage)
import System.FilePath ((</>))
import Data.Maybe (catMaybes)

bumpAll :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
bumpAll root config localPackages =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate (UpdatedConfig package updateConfig) = do
      cyanMessage . T.pack $ "Bumping version of package '" <> (Package.getLocalName . Daml.packageConfig $ package) <> "'"
      flip Daml.writeDamlConfig updateConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully updated!"
  in
    incrementPackages localPackages
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

incrementPackages :: [Daml.Package] -> IO [UpdatedConfig]
incrementPackages packages = catMaybes <$> mapM incrementPackage packages

incrementPackage :: Daml.Package -> IO (Maybe UpdatedConfig)
incrementPackage package@Daml.Package{damlConfig, packageConfig} =
  let
    name = Package.getLocalName packageConfig
    version = Daml.version damlConfig
    incrementConfig = Package.incrementVersion packageConfig
  in
    increment name version incrementConfig >>= \case
      Just newVersion -> pure . Just . UpdatedConfig package $ updateVersion damlConfig newVersion
      _ -> pure Nothing

-- | Checks if a tag/version already exists in the git repo
-- When it already exists, it will compute the next increment of the version.
increment :: String -> String -> IncrementVersion -> IO (Maybe String)
increment name version inc = Git.tagExists (name <> "/" <> version) >>= \case
  True -> increment' version inc >>= \version -> pure $ Just version
  _ -> pure Nothing

-- | Increments a specific part of a version.
increment' :: String -> IncrementVersion -> IO String
increment' version inc = case (inc, get version) of
  (MAJOR, x:_)        -> pure $ incrementString x <> ".0.0"
  (MINOR, x:y:_)      -> pure $ x <> "." <> incrementString y <> ".0"
  (PATCH, x:y:z:_)    -> pure $ x <> "." <> y <> "." <> incrementString z
  (SNAPSHOT, x:y:z:"99":d:i:_) -> do
    (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
    if d == show year <> show month <> show day then
      pure $ x <> "." <> y <> "." <> z <> ".99." <> d <> "." <> incrementString i
    else
      pure $ x <> "." <> y <> "." <> z <> ".99." <> show year <> show month <> show day <> ".1"
  (SNAPSHOT, x:y:z:_) -> do
    (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
    pure $ x <> "." <> y <> "." <> z <> ".99." <> show year <> show month <> show day <> ".1"
  _                   -> error $ "Unexpected version format. version=" <> version

-- | Extracts the version numbering from a versioning string, removing the period.
-- > get "123.45.67" = ["123", "45", "67"]
get :: String -> [String]
get = foldr (\c acc -> if c == '.' then []:acc else (c:head acc):tail acc) [[]]

-- | Increments a version/string by one.
-- Versions are enforced by daml to be integers.
incrementString :: String -> String
incrementString = show . (+) 1 . read

-- | Update the version of a daml config file.
updateVersion :: Daml.Config -> String -> Daml.Config
updateVersion config version = config { Daml.version = version }
