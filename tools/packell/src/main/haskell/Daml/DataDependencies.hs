-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Daml.DataDependencies (
    update
  , dryRun
  , validate
) where

import Colourista.IO (errorMessage, successMessage, boldMessage, redMessage, infoMessage, greenMessage, warningMessage, yellowMessage, skipMessage, cyanMessage)
import qualified Daml.Import as Import (getPackageModules)
import Daml.Source (Source(..), getSource)
import qualified Daml.Package as Daml (Package(..), damlConfig,  packageConfig)
import qualified Daml.Yaml as Daml (Config(..), damlConfigFile, source, version, writeDamlConfig)
import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete)
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
data UpdateImport = UpdateImport {
    package :: Daml.Package
  , updatedConfig :: Daml.Config
} deriving (Eq, Show)

-- | Updates data-dependencies of a list of packages matches the usage in their sources.
update :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
update root config localPackages =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate (UpdateImport package updateConfig) = do
      cyanMessage . T.pack $ "Updating package '" ++ (Package.getLocalName . Daml.packageConfig $ package) ++ "'"
      flip Daml.writeDamlConfig updateConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully updated!"
  in
    processDataDependencies root config localPackages
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

-- | Writes to console packages which require their imports to be updated.
dryRun :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
dryRun root config localPackages = -- If nothing, print success "nothing to update"
  processDataDependencies root config localPackages >>= mapM_ printUpdate
    where
      printUpdate (UpdateImport package updateConfig) = do
        warningMessage . T.pack $ "Package to update : " ++ (Package.getLocalName . Daml.packageConfig $ package)
        redMessage . T.pack $ "Current data dependencies :"
        mapM_ (redMessage . T.pack) $ L.concat . maybeToList . Daml.dataDependencies . Daml.damlConfig $ package
        cyanMessage . T.pack $ "Updated data dependencies :"
        mapM_ (cyanMessage . T.pack) $ L.concat . maybeToList . Daml.dataDependencies $ updateConfig
        putStr "\n"

-- | Validates if any package data-dependences requires updating. Throws an exception if any package requires updating.
validate :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
validate root config localPackages =
  processDataDependencies root config localPackages >>= \case
    [] -> successMessage . T.pack $ "All packages are up-to-date!"
    xs -> do
      errorMessage . T.pack $ show (length xs) ++ " package/s require updating."
      errorMessage . T.pack $ "Packages=[" ++ foldl f "" xs ++ "]."
      error "Run 'packell data-dependencies update' to resolve this error."
        where
        getPackageName = Package.getLocalName . Daml.packageConfig . package
        f acc p = if acc == "" then getPackageName p else acc <> ", " <> getPackageName p

-- | Process data dependencies for all provided packages.
processDataDependencies :: FilePath -> Package.Config -> [Daml.Package] -> IO [UpdateImport]
processDataDependencies root config allPackages = catMaybes <$> mapM (processDataDependency root config allPackages) allPackages

-- | Processes an individual package's data dependencies.
processDataDependency :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> IO (Maybe UpdateImport)
processDataDependency root config allPackages package = do
  damlModules <- Import.getPackageModules root package

  let
    newDataDependencies = generateDataDependencies config allPackages package damlModules
    currentDataDependenciesMaybe = Daml.dataDependencies . Daml.damlConfig $ package
    currentDamlConfig = Daml.damlConfig package

  pure $ case (currentDataDependenciesMaybe, newDataDependencies) of
     (Nothing, []) -> Nothing
     (Nothing, xs) -> Just . UpdateImport package $ updateDamlConfig currentDamlConfig xs
     (Just cur, xs)
      | cur == newDataDependencies -> Nothing
      | otherwise -> Just . UpdateImport package $ updateDamlConfig currentDamlConfig xs

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
    </> generateDarName ((Daml.name . Daml.damlConfig) package) ((Daml.version . Daml.damlConfig) package)

-- Generate a daml dar file name.
generateDarName :: String -> String -> String
generateDarName name version = name <> "-" <> version <> darExtension

-- Update the data-dependencies of a daml config file.
updateDamlConfig :: Daml.Config -> [FilePath] -> Daml.Config
updateDamlConfig config [] = config { Daml.dataDependencies = Nothing }
updateDamlConfig config xs = config { Daml.dataDependencies = Just xs }
