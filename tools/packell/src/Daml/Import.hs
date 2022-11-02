{-# LANGUAGE ScopedTypeVariables #-}

module Daml.Import where

import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete)
import Daml.Source (Source(..), getSource)
import qualified Daml.Package as Daml (Package(..), damlConfig, packageConfig)
import qualified Daml.Yaml as Daml (Config(..), source, version)
import qualified GHC.List as L (concat)
import qualified Package.Yaml as Package (Config(..), Remote(..), Local(..), local, getLocalBaseModule, getLocalName, getLocalRepoName, getRemoteBaseModule, getRemotePackages, getRemoteRepoName, path)
import System.Directory (listDirectory, makeAbsolute, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf)
import System.FilePattern.Directory (getDirectoryFiles, FilePattern)
import Daml.Yaml (Config(dataDependencies), writeDamlConfig, damlConfigFile)
import Data.Maybe (catMaybes)

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
updateImports root config localPackages = do
  -- results <- catMaybes <$> mapM (updateImport root config localPackages) localPackages
  results <- catMaybes <$> mapM (updateImport root config localPackages) [localPackages !! 14]
  -- print results
  let getDamlPath package = root </> (Package.path . Daml.packageConfig) package </> damlConfigFile
  mapM_ (\(UpdateImport package updateConfig) -> writeDamlConfig (getDamlPath package) updateConfig) results

  -- updateImport root config localPackages (localPackages !! 14) -- for testing - Account
  -- updateImport root config localPackages (localPackages !! 30) -- for testing - Contingent.Test
  -- updateImport root config localPackages (head localPackages) -- for testing

-- | Processes an individual package.
updateImport :: FilePath -> Package.Config -> [Daml.Package] -> Daml.Package -> IO (Maybe UpdateImport)
updateImport root config allLocalPackages localPackage = do
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
    currentDependenciesMaybe = Daml.dataDependencies . Daml.damlConfig $ localPackage

  pure $ currentDependenciesMaybe >>= \cur ->
      if cur == dataDependencies then
        Nothing
      else do
        let
          curDamlConfig = Daml.damlConfig localPackage
          updatedDamlConfig = curDamlConfig { dataDependencies = Just dataDependencies }
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

