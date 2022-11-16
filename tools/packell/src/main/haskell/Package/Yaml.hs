-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language DuplicateRecordFields #-}

module Package.Yaml (
    Config(..)
  , IncrementVersion(..)
  , Local(..)
  , LocalPackage(..)
  , LocalPackages(..)
  , Repo(..)
  , Remote(..)
  , RemotePackage(..)
  , RemotePackages(..)
  , T
  , readPackageYaml
  , getLocalBaseModule
  , getLocalName
  , getlocalPackages
  , getLocalRepo
  , getLocalRepoName
  , getRemoteBaseModule
  , getRemotePackages
  , getRemoteRepo
  , getRemoteRepoName
  , getRepoName
) where

import Control.Applicative
import Data.Yaml (FromJSON(parseJSON), (.:), Value(Object, String), decodeFileEither, ParseException, Parser, withArray, parseJSONList, withObject)
import qualified Data.Vector as V (toList)
import qualified Data.Text as T (unpack, toUpper)

type T = Config

-- | Repository details.
data Repo = Repo {
    host :: String
  , organisation :: String
  , name :: String
  } deriving (Eq, Show)

-- | Semantic Versioning (https://semver.org/)
data IncrementVersion
  = SNAPSHOT
  | PATCH
  | MINOR
  | MAJOR
  deriving (Eq, Show)

-- | A locally defined package.
data Local = Local {
    name :: String
  , path :: String
  , baseModule :: String
  , incrementVersion :: IncrementVersion
  } deriving (Eq, Show)

-- | A wrapper for a local package.
newtype LocalPackage = LocalPackage { localPackage :: Local }
  deriving (Eq, Show)

-- | A wrapper for a local packages.
data LocalPackages = LocalPackages {
    repo :: Repo
  , localPackages :: [LocalPackage]
  } deriving (Eq, Show)

-- | A remotely defined package.
data Remote = Remote {
    name :: String
  , repo :: Repo
  , baseModule :: String
  , tag :: String
  , darName :: String
  } deriving (Eq, Show)

-- | A wrapper for a remote packge.
newtype RemotePackage = RemotePackage { remotePackage :: Remote }
  deriving (Eq, Show)

newtype RemotePackages = RemotePackages { remotePackages :: [RemotePackage] }
  deriving (Eq, Show)

-- | Represents a package config file
data Config = Config {
    installDir :: String
  , remote :: RemotePackages
  , local :: LocalPackages
  } deriving (Eq, Show)

instance FromJSON Repo where
  parseJSON (Object v) =
    Repo <$>
      v .: "host" <*>
      v .: "organisation" <*>
      v .: "name"
  parseJSON _ = error "Cannot parse local from YAML"

instance FromJSON IncrementVersion where
  parseJSON (String s) =
    pure $ case T.toUpper s of
      "SNAPSHOT" -> SNAPSHOT
      "PATCH"    -> PATCH
      "MINOR"    -> MINOR
      "MAJOR"    -> MAJOR
      _          -> error $ "Unexpected incremental version type set in YAML. type='" ++ T.unpack s ++ "'"
  parseJSON _ = error "Cannot parse incremental version from YAML"

instance FromJSON Local where
  parseJSON (Object v) =
    Local <$>
      v .: "name" <*>
      v .: "path" <*>
      v .: "base-module" <*>
      v .: "increment-version"
  parseJSON _ = error "Cannot parse local from YAML"

instance FromJSON LocalPackage where
  parseJSON (Object v) = LocalPackage <$> v .: "package"
  parseJSON _          = error "Cannot parse local package from YAML"

instance FromJSON LocalPackages where
  parseJSON (Object v) =
    LocalPackages <$>
      v .: "repo" <*>
      v .: "packages"
  parseJSON _ = error "Cannot parse local package from YAML"

instance FromJSON Remote where
  parseJSON (Object v) =
    Remote <$>
      v .: "name" <*>
      v .: "repo" <*>
      v .: "base-module" <*>
      v .: "tag" <*>
      v .: "dar-name"
  parseJSON _ = error "Cannot parse remote from YAML"

instance FromJSON RemotePackage where
  parseJSON (Object v) = RemotePackage <$> v .: "package"
  parseJSON _          = error "Cannot parse remote package from YAML"

instance FromJSON RemotePackages where
  parseJSON (Object v) = RemotePackages <$> v .: "packages"
  parseJSON _          = error "Cannot parse remote package from YAML"

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
      v .: "package-install-dir" <*>
      v .: "remote" <*>
      v .: "local"
  parseJSON _ = error "Cannot parse daml config from YAML"

-- | Given a file path to the package config file, this will extract and return the contents.
readPackageYaml :: FilePath -> IO Config
readPackageYaml filePath = do
  packageConfig :: (Either ParseException Config) <- decodeFileEither filePath
  case packageConfig of
    Left exception -> error $ "Exception occured while reading package yaml. filePath=" ++ filePath ++ ", exception=" ++ show exception
    Right dc -> pure dc

-- | Extract the remote packages from a package config.
getRemotePackages :: Config -> [Remote]
getRemotePackages = map remotePackage . remotePackages . remote

-- | Extract the local packages from a package config.
getlocalPackages :: Config -> [Local]
getlocalPackages = map localPackage . localPackages . local

-- These functions are required due to multiple declarations of the same record fields.
-- We should refactor these out of the code base if possible.
getLocalBaseModule :: Local -> String
getLocalBaseModule = baseModule

getRemoteBaseModule :: Remote -> String
getRemoteBaseModule = baseModule

getRepoName :: Repo -> String
getRepoName = name

getRemoteRepo :: Remote -> Repo
getRemoteRepo = repo

getRemoteRepoName :: Remote -> String
getRemoteRepoName = getRepoName . getRemoteRepo

getLocalRepo :: LocalPackages -> Repo
getLocalRepo = repo

getLocalRepoName :: LocalPackages -> String
getLocalRepoName = getRepoName . getLocalRepo

getLocalName :: Local -> String
getLocalName = name
