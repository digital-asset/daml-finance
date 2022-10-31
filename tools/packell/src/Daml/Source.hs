{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Daml.Source where

import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete)
import qualified Daml.Package as Daml (Local(..), damlConfig, packageConfig)
import qualified Daml.Yaml as Daml (Config(..), source)
import qualified GHC.List as L (concat)
import qualified Package.Yaml as Package (Config(..), getLocalBaseModule, getRemoteBaseModule, getRemotePackages, path)
import System.Directory (listDirectory, makeAbsolute, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf)
import System.FilePattern.Directory (getDirectoryFiles, FilePattern)

damlFilePattern :: [FilePattern] = ["**/*.daml"]
-- | Import daml command
damlImport :: String = "import"

-- | Checks the dependencies of a list of packages matches the usage in their sources.
processPackages :: FilePath -> Package.Config -> [Daml.Local] -> IO ()
processPackages root config localPackages = -- mapM_ (processPackage packages) packages
  processPackage root config localPackages (localPackages !! 14) -- for testing

-- | Processes an individual package.
processPackage :: FilePath -> Package.Config -> [Daml.Local] -> Daml.Local -> IO ()
processPackage root config allLocalPackages localPackage = do
  let
    path = Package.path . Daml.packageConfig $ localPackage
    source = Daml.source . Daml.damlConfig $ localPackage
    sourceDir = root </> path </> source

  damlFiles <- map (sourceDir </>) <$> getDirectoryFiles sourceDir damlFilePattern
  damlImports <- L.concat <$> mapM getDamlImports damlFiles

  let
    damlModules = sort . nub $ map (getImportModule . words) damlImports
    localPackages = localPackage `delete` allLocalPackages
    getDependencies getBaseModule packages = nub $ foldl (\acc m -> acc ++ filter (flip isPrefixOf m . getBaseModule) packages) [] damlModules
    localDependencies = getDependencies (Package.getLocalBaseModule . Daml.packageConfig) localPackages
    remotePackages = Package.getRemotePackages config
    remoteDependencies = getDependencies Package.getRemoteBaseModule remotePackages

  print localPackage
  print damlModules
  print localDependencies
  print remoteDependencies
  pure ()

-- | For a Daml file, extract the import lines.
getDamlImports :: FilePath -> IO [String]
getDamlImports damlFile = getImportLines [] False . lines <$> readFile damlFile

-- | Gets the import block in a daml file - has shortcutting semantics.
-- Recursively process each line from the start of the file.
-- Once an import line is found, keep recursively processing each line until a line which is either not an import nor an empty line.
getImportLines :: [String] -> Bool -> [String] -> [String]
getImportLines r False (x:xs) = if hasImportPrefix (words x) then getImportLines (x:r) True xs else getImportLines r False xs
getImportLines r True  (x:xs) = if | hasImportPrefix (words x)          -> getImportLines (x:r) True xs
                                   | isEmptyOrMultiLineImport (words x) -> getImportLines r     True xs
                                   | otherwise                          -> r
getImportLines r _     []     = r

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
