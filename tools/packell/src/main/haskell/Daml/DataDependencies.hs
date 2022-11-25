-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Daml.DataDependencies (
    update
  , dryRun
  , validate
) where

import Colourista.IO (errorMessage, successMessage, boldMessage, redMessage, infoMessage, greenMessage, warningMessage, yellowMessage, skipMessage, cyanMessage)
import Control.Monad (when)
import qualified Daml.Import as Import (getPackageModules)
import Daml.Source (Source(..), getSource)
import qualified Daml.Package as Daml (Package(..), damlConfig,  packageConfig)
import qualified Daml.Version as Version (update)
import qualified Daml.Yaml as Daml (Config(..), damlConfigFile, source, version, writeDamlConfig)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete, (\\))
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text as T (pack)
import qualified GHC.List as L (concat)
import qualified Package.Yaml as Package (Config(..), Remote(..), Local(..), local, getLocalBaseModule, getLocalName, getLocalRepoName, getRemoteBaseModule, getRemotePackages, getRemoteRepoName, path)
import System.Directory (listDirectory, makeAbsolute, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf)
import System.FilePattern.Directory (getDirectoryFiles, FilePattern)

-- | The dar file extension.
darExtension :: String = ".dar"

-- | Details a daml config to update alongside the related package information.
data UpdatePackage = UpdatePackage {
    package :: Daml.Package
  , updatedConfig :: Daml.Config
} deriving (Eq, Show)

-- | Updates data-dependencies of a list of packages matches the usage in their sources.
update :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
update root config localPackages =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate (UpdatePackage package updateConfig) = do
      cyanMessage . T.pack $ "Updating package '" <> (Package.getLocalName . Daml.packageConfig $ package) <> "'"
      flip Daml.writeDamlConfig updateConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully updated!"
  in
    processDataDependencies root config localPackages
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

-- | Writes to console packages which require their imports to be updated.
dryRun :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
dryRun root config localPackages = do
  processDataDependencies root config localPackages >>= \case
    [] -> greenMessage . T.pack $ "All packages are up-to-date!"
    xs -> mapM_ printUpdate xs
  where
    printUpdate (UpdatePackage package updateConfig) = do
      let
        version = Daml.version . Daml.damlConfig $ package
        newVersion = Daml.version updateConfig
        dataDependencies = L.concat . maybeToList . Daml.dataDependencies . Daml.damlConfig $ package
        newDataDependencies = L.concat . maybeToList . Daml.dataDependencies $ updateConfig
      warningMessage . T.pack $ "Package to update : " <> (Package.getLocalName . Daml.packageConfig $ package)
      when (version /= newVersion) $ redMessage . T.pack $ "Current version : " <> version
      redMessage . T.pack $ "Removing data-dependencies :"
      mapM_ (redMessage . T.pack) $ dataDependencies \\ newDataDependencies
      when (version /= newVersion) $ cyanMessage . T.pack $ "Updated version : " <> newVersion
      cyanMessage . T.pack $ "Adding data-dependencies :"
      mapM_ (cyanMessage . T.pack) $ newDataDependencies \\ dataDependencies
      putStr "\n"

-- | Validates if any package data-dependences requires updating. Throws an exception if any package requires updating.
validate :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
validate root config localPackages =
  processDataDependencies root config localPackages >>= \case
    [] -> successMessage . T.pack $ "All packages are up-to-date!"
    xs -> do
      errorMessage . T.pack $ show (length xs) <> " package/s require updating."
      errorMessage . T.pack $ "Packages=[" <> foldl f "" xs <> "]."
      error "Run 'packell data-dependencies update' to resolve this error."
        where
        getPackageName = Package.getLocalName . Daml.packageConfig . package
        f acc p = if acc == "" then getPackageName p else acc <> ", " <> getPackageName p

-- | Process data dependencies for all provided packages.
processDataDependencies :: FilePath -> Package.Config -> [Daml.Package] -> IO [UpdatePackage]
processDataDependencies root config allPackages =
  reverse . snd <$> foldlM (processDataDependency root config) (allPackages, []) allPackages

-- | Processes a package and updates it's version if required - must be called in build order.
-- For each updated package, determine if the version requires bumping. If the version is bumped,
-- replace the original package from the list of overall packages with the updated package.
-- Note - To not depend on build order, iterate through the package list twice (either calling
--  processDataDependency' or searching through the data-dependencies of each package for the updated dependency)
processDataDependency :: FilePath -> Package.Config -> ([Daml.Package], [UpdatePackage]) -> Daml.Package -> IO ([Daml.Package], [UpdatePackage])
processDataDependency root config acc@(allPackages, updatedPackages) package =
  processDataDependency' root config allPackages package >>= \case
    Nothing             -> pure acc
    Just updatedPackage -> checkVersion updatedPackage
  where
    replacePackage newPackage = package { Daml.damlConfig = updatedConfig newPackage } : package `delete` allPackages
    checkVersion newPackage   = validateVersion newPackage >>= \case
      Nothing             -> pure (allPackages, newPackage : updatedPackages)
      Just updatedPackage -> pure (replacePackage updatedPackage, updatedPackage : updatedPackages)

-- | Processes an individual package's data dependencies.
processDataDependency' :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> IO (Maybe UpdatePackage)
processDataDependency' root config allPackages package =
  let
    newDataDependencies damlModules = generateDataDependencies config allPackages package damlModules
    currentDamlConfig = Daml.damlConfig package
    currentDataDependenciesMaybe = Daml.dataDependencies currentDamlConfig
    justUpdatePackage xs = Just . UpdatePackage package $ updateDamlDataDependencies currentDamlConfig xs
  in
    Import.getPackageModules root package >>= \damlModules ->
      pure $ case (currentDataDependenciesMaybe, newDataDependencies damlModules) of
        (Nothing, []) -> Nothing
        (Nothing, xs) -> justUpdatePackage xs
        (Just cur, xs)
          | cur == xs -> Nothing
          | otherwise -> justUpdatePackage xs

-- | Updates a package version, if required.
validateVersion :: UpdatePackage -> IO (Maybe UpdatePackage)
validateVersion updatedPackage@UpdatePackage{package, updatedConfig} =
  let
    version = Daml.version updatedConfig
    packageConfig = Daml.packageConfig package
    incrementVersion = Package.incrementVersion packageConfig
    name = Package.getLocalName packageConfig
  in
    Version.update name version incrementVersion >>= \case
      Just version -> pure . Just . UpdatePackage package $ updateDamlVersion updatedConfig version
      _ -> pure Nothing

-- | Creates the data dependencies for a package based of the sourced daml modules.
generateDataDependencies :: Package.Config -> [Daml.Package] -> Daml.Package -> [String] -> [FilePath]
generateDataDependencies config allPackages package damlModules =
  let
    remotePackages = Package.getRemotePackages config
    localPackages = package `delete` allPackages
    getDataDependencies getBaseModule packages = nub $ foldl (\acc m -> acc ++ filter (flip isPrefixOf m . getBaseModule) packages) [] damlModules
    remoteDataDependencies = getDataDependencies Package.getRemoteBaseModule remotePackages
    localDataDependencies = getDataDependencies (Package.getLocalBaseModule . Daml.packageConfig) localPackages
  in
    sort $ map (generateRemoteDependency config) remoteDataDependencies ++ map (generateLocalDependency config) localDataDependencies

-- | Generate a data-dependency for remote packages.
-- Format is <installDir>/<repo_name>/<tag>/<darname>
generateRemoteDependency :: Package.Config -> Package.Remote -> FilePath
generateRemoteDependency config remote =
  Package.installDir config
    </> Package.getRemoteRepoName remote
    </> Package.tag remote
    </> Package.darName remote

-- | Generate a data-dependency for local packages.
-- Format is <installDir>/<repo_name>/<package_name>/<package_version>/<darname>
generateLocalDependency :: Package.Config -> Daml.Package -> String
generateLocalDependency config package =
  Package.installDir config
    </> Package.getLocalRepoName (Package.local config)
    </> (Package.getLocalName . Daml.packageConfig) package
    </> (Daml.version . Daml.damlConfig) package
    </> generateDarName (Daml.name . Daml.damlConfig $ package) (Daml.version . Daml.damlConfig $ package)

-- | Generate a daml dar file name.
generateDarName :: String -> String -> String
generateDarName name version = name <> "-" <> version <> darExtension

-- | Update the data-dependencies of a daml config file.
updateDamlDataDependencies :: Daml.Config -> [FilePath] -> Daml.Config
updateDamlDataDependencies config [] = config { Daml.dataDependencies = Nothing }
updateDamlDataDependencies config xs = config { Daml.dataDependencies = Just xs }

-- | Update the version of a daml config file.
updateDamlVersion :: Daml.Config -> String -> Daml.Config
updateDamlVersion config version = config { Daml.version = version }
