-- Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Daml.Package (
    T
  , Package(..)
  , getLocalPackages
  , getLocalPackage
) where

import Control.Monad (filterM)
import Data.List (zipWith4)
import qualified Daml.Yaml as Daml (Config(..), damlConfigFile, readDamlConfig)
import qualified Package.Yaml as Package (Config(..), Local, getlocalPackages, path)
import System.FilePath ((</>), splitDirectories)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute, makeRelativeToCurrentDirectory)

type T = Package

-- | Represents an individual local build package config along with its daml config.
data Package = Package {
    packageConfig :: Package.Local
  , damlConfig :: Daml.Config
} deriving (Eq, Show)

-- | For a provided package config, extract the daml config per local package.
getLocalPackages :: Package.Config -> FilePath -> IO [Package]
getLocalPackages config packageRoot =
  mapM (`getLocalPackage` packageRoot) $ Package.getlocalPackages config

-- | For a given root file path, extract the necessary package information.
getLocalPackage :: Package.Local -> FilePath -> IO Package
getLocalPackage localPackage packageRoot = do
  path <- makeAbsolute (packageRoot </> Package.path localPackage)
  damlConfig <- Daml.readDamlConfig $ path </> Daml.damlConfigFile
  pure $ Package localPackage damlConfig
