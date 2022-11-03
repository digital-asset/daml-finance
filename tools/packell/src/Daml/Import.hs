-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Daml.Import (
    updateImports
  , printImportsToUpdate
  , validateImports
) where

import Colourista.IO (errorMessage, successMessage, boldMessage, redMessage, infoMessage, greenMessage, warningMessage, yellowMessage, skipMessage, cyanMessage)
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

-- | The daml syntax to import a module.
damlImport :: String = "import"
-- | The dar file extension.
darExtension :: String = ".dar"

-- | Details a daml config to update alongside the related package information.
data UpdateImport = UpdateImport {
    package :: Daml.Package
  , updatedConfig :: Daml.Config
} deriving (Eq, Show)

-- | Checks the dependencies of a list of packages matches the usage in their sources.
updateImports :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
updateImports root config localPackages =
  let
    getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> Daml.damlConfigFile
    writeUpdate (UpdateImport package updateConfig) = do
      cyanMessage . T.pack $ "Updating package '" ++ (Package.getLocalName . Daml.packageConfig $ package) ++ "'"
      flip Daml.writeDamlConfig updateConfig $ getDamlPath package
    writeSuccessMessage = successMessage . T.pack $ "Packages successfully updated!"
  in
    processImports root config localPackages
      >>= mapM_ writeUpdate
      >>= const writeSuccessMessage

-- | Prints out packages which require their imports to be updated.
printImportsToUpdate :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
printImportsToUpdate root config localPackages = -- If nothing, print success "nothing to update"
  processImports root config localPackages >>= mapM_ printUpdate
    where
      printUpdate (UpdateImport package updateConfig) = do
        warningMessage . T.pack $ "Package to update : " ++ (Package.getLocalName . Daml.packageConfig $ package)
        redMessage . T.pack $ "Current data dependencies :"
        mapM_ (redMessage . T.pack) $ L.concat . maybeToList . Daml.dataDependencies . Daml.damlConfig $ package
        cyanMessage . T.pack $ "Updated data dependencies :"
        mapM_ (cyanMessage . T.pack) $ L.concat . maybeToList . Daml.dataDependencies $ updateConfig
        putStr "\n"

-- | Throws an exception if any package requires updating.
validateImports :: FilePath -> Package.Config -> [Daml.Package] -> IO ()
validateImports root config localPackages =
  processImports root config localPackages >>= \case
    [] -> successMessage . T.pack $ "All packages are up-to-date!"
    xs -> do
      errorMessage . T.pack $ show (length xs) ++ " package/s require updating."
      errorMessage . T.pack $ "Packages=[" ++ foldl f "" xs ++ "]."
      error "Run 'packell imports update' to resolve this error."
        where
        getPackageName = Package.getLocalName . Daml.packageConfig . package
        f acc p = if acc == "" then getPackageName p else acc <> ", " <> getPackageName p

-- | Process all provided imports.
processImports :: FilePath -> Package.Config -> [Daml.Package] -> IO [UpdateImport]
processImports root config localPackages = catMaybes <$> mapM (processImport root config localPackages) localPackages

-- | Processes an individual package.
processImport :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> IO (Maybe UpdateImport)
processImport root config allLocalPackages localPackage = do
  Source _ damlFiles <- getSource root localPackage
  damlImports <- L.concat <$> mapM getDamlImports damlFiles

  let
    damlModules = sort . nub $ map (getImportModule . words) damlImports
    remotePackages = Package.getRemotePackages config
    localPackages = localPackage `delete` allLocalPackages
    getDataDependencies getBaseModule packages = nub $ foldl (\acc m -> acc ++ filter (flip isPrefixOf m . getBaseModule) packages) [] damlModules
    remoteDataDependencies = getDataDependencies Package.getRemoteBaseModule remotePackages
    localDataDependencies = getDataDependencies (Package.getLocalBaseModule . Daml.packageConfig) localPackages
    dataDependencies = sort $ map (generateRemoteDependency config) remoteDataDependencies ++ map (generateLocalDependency config) localDataDependencies
    currentDataDependenciesMaybe = Daml.dataDependencies . Daml.damlConfig $ localPackage

  pure $ currentDataDependenciesMaybe >>= \cur ->
      if cur == dataDependencies then
        Nothing
      else do
        let
          curDamlConfig = Daml.damlConfig localPackage
          updatedDamlConfig = curDamlConfig { Daml.dataDependencies = Just dataDependencies }
        Just (UpdateImport localPackage updatedDamlConfig)

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

-- | For a Daml file, extract the import lines.
getDamlImports :: FilePath -> IO [String]
getDamlImports damlFile = getImportLines [] False . lines <$> readFile damlFile

-- | Gets the import block in a daml file - has shortcutting semantics.
-- Recursively process each line from the start of the file.
-- Once an import line is found, keep recursively processing each line until a line which is either not an import nor an empty line.
getImportLines :: [String] -> Bool -> [String] -> [String]
getImportLines res _     []     = res
getImportLines res False (x:xs) = if hasImportPrefix (words x) then getImportLines (x:res) True xs else getImportLines res False xs
getImportLines res True  (x:xs)
  | hasImportPrefix (words x)          = getImportLines (x:res) True xs
  | isEmptyOrMultiLineImport (words x) = getImportLines res     True xs
  | otherwise                          = res

-- | Match the first word in a line is "import".
hasImportPrefix :: [String] -> Bool
hasImportPrefix [] = False
hasImportPrefix (x:xs) = x == damlImport

-- | Checks if the line is empty or if its a part of a multi-line explicit import list
isEmptyOrMultiLineImport :: [String] -> Bool
isEmptyOrMultiLineImport [] = True
isEmptyOrMultiLineImport [x] = head x == ',' || last x `elem` [',', ')']
isEmptyOrMultiLineImport (x:xs)
  | x == "," = True
  | head x == ',' = True
  | last (last xs) `elem` [',', ')'] = True
isEmptyOrMultiLineImport _ = False

-- | For a matched line, return the module.
-- This is the second 'word' in an import line unless its a haskell-like "qualifed" import.
getImportModule :: [String] -> String
getImportModule (_:"qualified":x:_) = x
getImportModule (_:x:_) = x
getImportModule i = error $ "Import string in unexpected format. string=" ++ unwords i
