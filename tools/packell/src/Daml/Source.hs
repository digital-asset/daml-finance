{-# LANGUAGE ScopedTypeVariables #-}

module Daml.Source where

import Control.Monad (filterM)
import Data.Functor ((<&>))
import Data.List (sort, group, nub, isPrefixOf, find, delete)
import qualified Daml.Package as Daml (Package(..), damlConfig, packageConfig)
import qualified Daml.Yaml as Daml (Config(..), source)
import qualified GHC.List as L (concat)
import qualified Package.Yaml as Package (Config(..), getLocalBaseModule, getRemoteBaseModule, getRemotePackages, path)
import System.Directory (listDirectory, makeAbsolute, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), isExtensionOf)
import System.FilePattern.Directory (getDirectoryFiles, FilePattern)

-- | File pattern for searching recursively for all daml files across multiple directories.
damlFilePattern :: [FilePattern] = ["**/*.daml"]

-- The sources belonging to a package.
data Source = Source {
    packageConfig :: Daml.Package
  , sources :: [FilePath]
  } deriving (Eq, Show)

-- | Gets the sources for a list of local packages.
getSources :: FilePath -> [Daml.Package] -> IO [Source]
getSources root = mapM (getSource root)

-- | Processes an individual package.
getSource :: FilePath -> Daml.Package -> IO Source
getSource root localPackage = do
  let
    path = Package.path . Daml.packageConfig $ localPackage
    source = Daml.source . Daml.damlConfig $ localPackage
    sourceDir = root </> path </> source

  damlFiles <- map (sourceDir </>) <$> getDirectoryFiles sourceDir damlFilePattern
  pure $ Source localPackage damlFiles
