-- Copyright (c) 2024 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Daml.Version (
    bumpAll
  , dryRun
  , increment
  , update
  , updateVersion
  , validate
) where

import qualified Daml.Package as Daml (Package(..))
import Daml.Package (Package(damlConfig, packageConfig))
import qualified Data.Text as T (pack)
import Daml.Types (UpdatedConfig(..))
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime(utctDay), getCurrentTime)
import Package.Yaml (IncrementVersion(SNAPSHOT, PATCH, MINOR, MAJOR), LocalPackage (localPackage))
import qualified Package.Yaml as Package (Config(..), getLocalName, incrementVersion, Local (path))
import qualified Git.Commands as Git (tagExists, hasDiff)
import qualified Daml.Yaml as Daml (Config, version, damlConfigFile, writeDamlConfig)
import Colourista (cyanMessage, successMessage, errorMessage, greenMessage, warningMessage, redMessage)
import System.FilePath ((</>))
import Data.Maybe (catMaybes)
import Daml.Source (Source(Source, sources), getSource)

-- | Updates versions of a list of packages based off their commits.
update :: FilePath -> [Daml.Package] -> IO ()
update root localPackages =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate UpdatedConfig{package, updatedConfig} = do
      cyanMessage . T.pack $ "Updating package '" <> (Package.getLocalName . Daml.packageConfig $ package) <> "'"
      flip Daml.writeDamlConfig updatedConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully processed!"
    updatePackages packages newPackage = undefined
  in
    processPackagesCommits root localPackages
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

-- | Writes to console packages which require their version to be updated based off their commits.
dryRun :: FilePath -> [Daml.Package] -> IO ()
dryRun root localPackages =
  processPackagesCommits root localPackages >>= \case
    [] -> greenMessage . T.pack $ "All packages are up-to-date!"
    xs -> mapM_ printUpdate xs
  where
    printUpdate UpdatedConfig{package, updatedConfig} = do
      let
        version = Daml.version . Daml.damlConfig $ package
        newVersion = Daml.version updatedConfig
      warningMessage . T.pack $ "Package to update : " <> (Package.getLocalName . Daml.packageConfig $ package)
      redMessage . T.pack $ "Current version : " <> version
      cyanMessage . T.pack $ "New version : " <> newVersion
      putStr "\n"

-- | Validates if any package has commits which requires the version to be bumped.
-- Throws an exception if any package requires updating.
validate :: FilePath -> [Daml.Package] -> IO ()
validate root localPackages =
  processPackagesCommits root localPackages >>= \case
    [] -> successMessage . T.pack $ "All packages are up-to-date!"
    xs -> do
      errorMessage . T.pack $ show (length xs) <> " package/s require updating."
      errorMessage . T.pack $ "Packages=[" <> foldl f "" xs <> "]."
      error "Run 'packell versioning update' to resolve this error."
        where
          getPackageName = Package.getLocalName . Daml.packageConfig . package
          f acc p = if acc == "" then getPackageName p else acc <> ", " <> getPackageName p

-- | Bump the versions of packages, either only bumping versions which exist as tags in git or
-- by forcing the updating of the version.
bumpAll :: FilePath -> [Daml.Package] -> Bool -> IO ()
bumpAll root localPackages force =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate (UpdatedConfig package updateConfig) = do
      cyanMessage . T.pack $ "Updating the version of package '"
        <> (Package.getLocalName . Daml.packageConfig $ package) <> "'"
        <> " from '" <> (Daml.version . Daml.damlConfig $ package) <> "'"
        <> " to " <> Daml.version updateConfig <> "'"
      flip Daml.writeDamlConfig updateConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully processed!"
  in
    incrementPackagesVersion localPackages force
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

-- | Processes the commits of a list of packages..
processPackagesCommits :: FilePath -> [Daml.Package] -> IO [UpdatedConfig]
processPackagesCommits root localPackages = catMaybes <$> mapM (processPackageCommit root) localPackages

-- | Updates the version of a package when a new commit is detected against the package and the
-- package version exists as a tag in git.
processPackageCommit :: FilePath -> Daml.Package -> IO (Maybe UpdatedConfig)
processPackageCommit root localPackage@Daml.Package{damlConfig, packageConfig} =
  let
    name = Package.getLocalName packageConfig
    version = Daml.version damlConfig
    tag = name <> "/" <> version
  in
    incrementPackageVersion False localPackage >>= \case
      updatedConfig@(Just _) ->
        getSource root localPackage
          >>= \Source{sources} -> Git.hasDiff tag sources
          >>= \res -> pure $ if res then updatedConfig else Nothing
      _ -> pure Nothing

-- | Increment a set of packages.
incrementPackagesVersion :: [Daml.Package] -> Bool -> IO [UpdatedConfig]
incrementPackagesVersion packages force = catMaybes <$> mapM (incrementPackageVersion force) packages

-- | Increment a specific package.
-- If force is set to true, the package version is always updated. Otherwise a check against the tag
-- will be performed.
incrementPackageVersion :: Bool -> Daml.Package -> IO (Maybe UpdatedConfig)
incrementPackageVersion force package@Daml.Package{damlConfig, packageConfig} =
  let
    name = Package.getLocalName packageConfig
    version = Daml.version damlConfig
    incrementConfig = Package.incrementVersion packageConfig
    updateConfig = pure . Just . UpdatedConfig package . updateVersion damlConfig
  in
    if force then
      increment' version incrementConfig >>= updateConfig
    else
      increment name version incrementConfig >>= \case
        Just newVersion -> updateConfig newVersion
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
