-- Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
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
import Daml.Types (UpdatedConfig(..))
import qualified Daml.Version as Version (increment, updateVersion)
import qualified Daml.Yaml as Daml (Config(..), damlConfigFile, source, version, writeDamlConfig)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete, (\\), isInfixOf)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text as T (pack)
import qualified GHC.List as L (concat)
import qualified Package.Yaml as Package (Config(..), Remote(..), Local(..), local, getLocalBaseModule, getLocalName, getLocalRepoName, getRemoteBaseModule, getRemotePackages, getRemoteRepoName, path)
import System.Directory (listDirectory, makeAbsolute, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf, splitDirectories, joinPath)
import System.FilePattern.Directory (getDirectoryFiles, FilePattern)

-- | The dar file extension.
darExtension :: String = ".dar"

-- | Checks whether a dependency path contains one of these two "splice" substrings.
isSpliceDep :: FilePath -> Bool
isSpliceDep p =
  "splice-api-token-holding-v1"   `isInfixOf` p ||
  "splice-api-token-metadata-v1" `isInfixOf` p


-- | Updates data-dependencies of a list of packages matches the usage in their sources.
update :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
update root config localPackages =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate UpdatedConfig{package, updatedConfig} = do
      cyanMessage . T.pack $ "Updating package '" <> (Package.getLocalName . Daml.packageConfig $ package) <> "'"
      flip Daml.writeDamlConfig updatedConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully processed!"
  in
    processDataDependencies root config localPackages
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

-- | Writes to console packages which require their imports to be updated.
dryRun :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
dryRun root config localPackages =
  processDataDependencies root config localPackages >>= \case
    [] -> greenMessage . T.pack $ "All packages are up-to-date!"
    xs -> mapM_ printUpdate xs
  where
    printUpdate UpdatedConfig{package, updatedConfig} = do
      let
        version = Daml.version . Daml.damlConfig $ package
        newVersion = Daml.version updatedConfig
        dataDependencies = L.concat . maybeToList . Daml.dataDependencies . Daml.damlConfig $ package
        newDataDependencies = L.concat . maybeToList . Daml.dataDependencies $ updatedConfig
      warningMessage . T.pack $ "Package to update : " <> (Package.getLocalName . Daml.packageConfig $ package)
      when (version /= newVersion) $ redMessage . T.pack $ "Current version : " <> version
      redMessage . T.pack $ "Removing data-dependencies :"
      mapM_ (redMessage . T.pack) $ dataDependencies \\ newDataDependencies
      when (version /= newVersion) $ cyanMessage . T.pack $ "Updated version : " <> newVersion
      cyanMessage . T.pack $ "Adding data-dependencies :"
      mapM_ (cyanMessage . T.pack) $ newDataDependencies \\ dataDependencies
      putStr "\n"

-- | Validates if any package data-dependences requires updating.
-- Throws an exception if any package requires updating.
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
processDataDependencies :: FilePath -> Package.Config -> [Daml.Package] -> IO [UpdatedConfig]
processDataDependencies root config allPackages =
  reverse . snd <$> foldlM (processDataDependency root config) (allPackages, []) allPackages

-- | Processes a package and updates it's version if required - must be called in build order.
-- For each updated package, determine if the version requires bumping. If the version is bumped,
-- replace the original package from the list of overall packages with the updated package.
-- Note - To not depend on build order, iterate through the package list twice (either calling
--  processDataDependency' or searching through the data-dependencies of each package for the updated dependency)
processDataDependency :: FilePath -> Package.Config -> ([Daml.Package], [UpdatedConfig]) -> Daml.Package -> IO ([Daml.Package], [UpdatedConfig])
processDataDependency root config acc@(allPackages, updatedPackages) package =
  processDataDependency' root config allPackages package >>= \case
    Nothing             -> pure acc
    Just updatedPackage -> checkVersion updatedPackage
  where
    replacePackage newPackage = package { Daml.damlConfig = updatedConfig newPackage } : package `delete` allPackages
    checkVersion newPackage   = validateVersion newPackage >>= \case
      Nothing             -> pure (allPackages, newPackage : updatedPackages)
      Just updatedPackage -> pure (replacePackage updatedPackage, updatedPackage : updatedPackages)

-- | Processes an individual package's data dependencies and determines whether a package needs updating.
processDataDependency' :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> IO (Maybe UpdatedConfig)
processDataDependency' root config allPackages package =
  let
    newDataDependencies damlModules = generateDataDependencies config allPackages package damlModules
    currentDamlConfig = Daml.damlConfig package
    currentDataDependenciesMaybe = Daml.dataDependencies currentDamlConfig
    justUpdatePackage xs = Just . UpdatedConfig package $ updateDamlDataDependencies currentDamlConfig xs
  in
    -- Returns every DAML module used in the package.
    Import.getPackageModules root package >>= \damlModules ->
      pure $ case (currentDataDependenciesMaybe, newDataDependencies damlModules) of
        (Nothing, []) -> Nothing
        (Nothing, xs) -> justUpdatePackage xs
        (Just cur, xs) ->
          let
            -- Extracts any Splice dependencies from the existing list
            spliceCur = filter isSpliceDep cur
            merged = sort . nub $ xs ++ spliceCur

            norm :: [FilePath] -> [FilePath]
            norm = sort . nub
          in
            if norm cur == norm merged
              then Nothing
              else justUpdatePackage merged

-- | Updates a package version, if required.
validateVersion :: UpdatedConfig -> IO (Maybe UpdatedConfig)
validateVersion UpdatedConfig{package, updatedConfig} =
  let
    version = Daml.version updatedConfig
    packageConfig = Daml.packageConfig package
    incrementVersion = Package.incrementVersion packageConfig
    name = Package.getLocalName packageConfig
  in
    Version.increment name version incrementVersion >>= \case
      Just version -> pure . Just . UpdatedConfig package $ Version.updateVersion updatedConfig version
      _ -> pure Nothing

-- Compute relative path manually by counting directory components
computeRelativeDataDep :: FilePath -> FilePath -> FilePath
computeRelativeDataDep pkgDir depDar =
  let
    pkgParts = splitDirectories pkgDir
    darParts = splitDirectories depDar

    dropCommonPrefix (x:xs) (y:ys)
      | x == y = dropCommonPrefix xs ys
    dropCommonPrefix xs ys = (xs, ys)

    (pkgRest, darRest) = dropCommonPrefix pkgParts darParts

    upSteps = replicate (length pkgRest) ".."
    final = upSteps ++ darRest
  in
    joinPath final


-- | Creates the data dependencies for a package based of the sourced daml modules.
generateDataDependencies :: Package.Config -> [Daml.Package] -> Daml.Package -> [String] -> [FilePath]
generateDataDependencies config allPackages package damlModules =
  let
    -- Directory of this package (e.g. main/daml/Daml.Finance.Data.V4)
    pkgDir = "package" </> Package.path (Daml.packageConfig package)

    remotePackages = Package.getRemotePackages config
    localPackages  = package `delete` allPackages

    getDataDependencies getBaseModule packages = nub $ foldl (\acc m -> acc ++ filter (flip isPrefixOf m . getBaseModule) packages) [] damlModules

    remoteDataDependencies = getDataDependencies Package.getRemoteBaseModule remotePackages
    localDataDependencies = getDataDependencies (Package.getLocalBaseModule . Daml.packageConfig) localPackages
  in
    sort $ map (generateRemoteDependency config pkgDir) remoteDataDependencies ++ map (generateLocalDependency  config pkgDir) localDataDependencies

-- | Generate a data-dependency for remote packages.
-- Format is <installDir>/<repo_name>/<tag>/<darname>
generateRemoteDependency :: Package.Config -> FilePath -> Package.Remote -> FilePath
generateRemoteDependency config pkgDir remote =
  computeRelativeDataDep pkgDir fullDarPath
  where
    fullDarPath = 
      Package.installDir config
        </> Package.getRemoteRepoName remote
        </> Package.tag remote
        </> Package.darName remote

-- | Generate a data-dependency for local packages.
-- <installDir>/<localRepo>/<packageName>/<version>/<darName>
generateLocalDependency :: Package.Config -> FilePath -> Daml.Package -> FilePath
generateLocalDependency config pkgDir pkg =
  computeRelativeDataDep pkgDir fullDarPath
  where
    pkgConfig = Daml.damlConfig pkg
    pkgName = Daml.name pkgConfig
    pkgVersion = Daml.version pkgConfig

    fullDarPath = 
      "package"
      </> Package.path (Daml.packageConfig pkg)
      </> ".daml/dist"
      </> generateDarName pkgName pkgVersion


-- | Generate a daml dar file name.
generateDarName :: String -> String -> FilePath
generateDarName name version = name <> "-" <> version <> darExtension

-- | Update the data-dependencies of a daml config file.
updateDamlDataDependencies :: Daml.Config -> [FilePath] -> Daml.Config
updateDamlDataDependencies config [] = config { Daml.dataDependencies = Nothing }
updateDamlDataDependencies config xs = config { Daml.dataDependencies = Just xs }