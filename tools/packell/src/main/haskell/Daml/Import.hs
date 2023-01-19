-- Copyright (c) 2023 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}

module Daml.Import (
    -- PackageImports(..)
    getPackageModules
  , getDamlImports
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

-- | Acquire modules utilised in a package based off their daml imports.
getPackageModules :: FilePath -> Daml.Package -> IO [String]
getPackageModules root localPackage = do
  Source _ damlFiles <- getSource root localPackage
  damlImports <- L.concat <$> mapM getDamlImports damlFiles

  pure . sort . nub $ map (getImportModule . words) damlImports

-- | For a Daml file, extract the full import lines.
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
