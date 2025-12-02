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
import Data.Char (toLower, isUpper, chr)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete, (\\), stripPrefix, intercalate, isInfixOf)
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Text as T (pack)
import qualified GHC.List as L (concat)
import qualified Package.Yaml as Package (Config(..), Remote(..), Local(..), local, getLocalBaseModule, getLocalName, getLocalRepoName, getRemoteBaseModule, getRemotePackages, getRemoteRepoName, path)
import System.Directory (listDirectory, makeAbsolute, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf, makeRelative, takeDirectory, splitDirectories, joinPath)
import System.FilePattern.Directory (getDirectoryFiles, FilePattern)
import Debug.Trace (trace)

-- | The dar file extension.
darExtension :: String = ".dar"

-- | Convert a dotted CamelCase package name (e.g. "Daml.Finance.Util.V3")
-- into a dash-lowercase DAR filename prefix (e.g. "daml-finance-util-v3").
toDashLower :: String -> String
toDashLower s =
  stripDash $
    intercalate "-" (map fixSpecial (words (map dotToSpace s)))
  where
    dotToSpace '.' = ' '
    dotToSpace c = c

    -- Special handling: keep StructuredProduct unsplit
    fixSpecial seg
      | map toLower seg == "structuredproduct" = "structuredproduct"
      | otherwise = camelToDash seg

    camelToDash [] = []
    camelToDash (c:cs) = toLower c : go cs

    go [] = []
    go (x:xs)
      | isUpper x = '-' : toLower x : go xs
      | otherwise = toLower x : go xs

    stripDash ('-':xs) = xs
    stripDash xs = xs


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
    Nothing -> pure acc
    Just updatedPackage -> checkVersion updatedPackage
  where
    replacePackage newPackage = package { Daml.damlConfig = updatedConfig newPackage } : package `delete` allPackages
    checkVersion newPackage   = validateVersion newPackage >>= \case
      Nothing  -> pure (allPackages, newPackage : updatedPackages)
      Just updatedPackage -> pure (replacePackage updatedPackage, updatedPackage : updatedPackages)

-- | Processes an individual package's data dependencies.
processDataDependency' :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> IO (Maybe UpdatedConfig)
processDataDependency' root config allPackages package = do
  damlModules <- Import.getPackageModules root package

  putStrLn ("DEBUG: PACKAGE = " <> Package.getLocalName (Daml.packageConfig package))
  putStrLn ("DEBUG: MODULES = " <> show damlModules)

  let
    -- current config + deps
    currentConfig = Daml.damlConfig package
    currentDeps   = maybe [] id (Daml.dataDependencies currentConfig)

    autoDeps :: [FilePath]
    autoDeps =
      generateDataDependencies root config allPackages package damlModules

    -- detect splice DARs (must be preserved)
    isSpliceDep p =
         "splice-api-token-holding-v1"   `isInfixOf` p
      || "splice-api-token-metadata-v1" `isInfixOf` p

    spliceDeps = filter isSpliceDep currentDeps
    newDeps = sort . nub $ autoDeps ++ spliceDeps
    updatedConfig' = updateDamlDataDependencies currentConfig newDeps

  -- Skip if nothing changed
  if currentDeps == newDeps
    then pure Nothing
    else pure (Just (UpdatedConfig package updatedConfig'))



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

    -- drop the common prefix of directories
    dropCommon (x:xs) (y:ys)
      | x == y = dropCommon xs ys
    dropCommon xs ys = (xs, ys)

    (pkgRest, darRest) = dropCommon pkgParts darParts

    ups = replicate (length pkgRest) ".."
    final = ups ++ darRest
  in
    joinPath final


-- | Creates the data dependencies for a package based of the sourced daml modules.
generateDataDependencies :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> [String]-> [FilePath]
generateDataDependencies root config allPkgs pkg usedModules =
  trace ("DEBUG: genDataDeps for " <> Package.getLocalName (Daml.packageConfig pkg)
          <> "\nUSED MODULES = " <> show usedModules) $

  let
    remotePkgs = Package.getRemotePackages config
    localPkgs  = pkg `delete` allPkgs

    -- Select packages where the base module is a prefix of a used module
    selectDeps getBase pkgs =
      let 
        matches =
          nub [ p | m <- usedModules, p <- pkgs, getBase p `isPrefixOf` m ]
          
      in trace ("DEBUG: matching deps for base="
        <> show (map getBase pkgs)
        <> " => "
        <> show (nub matches))  -- Here too
              matches

    remoteDeps = selectDeps Package.getRemoteBaseModule remotePkgs
    localDeps  = selectDeps (Package.getLocalBaseModule . Daml.packageConfig) localPkgs

  in sort $
        map (generateRemoteDependency root config pkg) remoteDeps
     ++ map (generateLocalDependency  root pkg) localDeps


-- | Generate a data-dependency for remote packages (.lib, lib, etc.).
-- From daml.yaml directory (package/<Package.path>): go relatively to
-- the install dir (e.g. .lib/<repo>/<tag>/<dar>).
generateRemoteDependency :: FilePath -> Package.Config -> Daml.Package -> Package.Remote -> FilePath
generateRemoteDependency root config currentPkg remote =
  let
    pkgPathSegments = splitDirectories (Package.path (Daml.packageConfig currentPkg))
    ups = replicate (1 + length pkgPathSegments) ".."
    relPrefix = joinPath ups
  in
    relPrefix
      </> Package.installDir config
      </> Package.getRemoteRepoName remote
      </> Package.tag remote
      </> Package.darName remote

-- | Generate a data-dependency for local packages.
-- We work with paths relative to repo root; the relative path between
-- "package/main/daml/..." dirs is what we want in daml.yaml.
generateLocalDependency :: FilePath -> Daml.Package -> Daml.Package -> FilePath
generateLocalDependency root currentPkg depPkg =
  let
    currentDir = root </> Package.path (Daml.packageConfig currentPkg)

    depCfg = Daml.packageConfig depPkg
    version = Daml.version (Daml.damlConfig depPkg)
    repoName = Package.getLocalName depCfg      -- e.g. Daml.Finance.Interface.Util.V3
    dashName = toDashLower repoName                 -- eg: daml-finance-intercae-util-v3
    darName = dashName <> "-" <> version <> darExtension

    depDar =
      root
        </> Package.path depCfg
        </> ".daml" </> "dist" </> darName

  in
    computeRelativeDataDep currentDir depDar


-- | Generate a daml dar file name.
generateDarName :: String -> String -> String
generateDarName name version = name <> "-" <> version <> darExtension

-- | Update the data-dependencies of a daml config file.
updateDamlDataDependencies :: Daml.Config -> [FilePath] -> Daml.Config
updateDamlDataDependencies config [] = config { Daml.dataDependencies = Nothing }
updateDamlDataDependencies config xs = config { Daml.dataDependencies = Just xs }
