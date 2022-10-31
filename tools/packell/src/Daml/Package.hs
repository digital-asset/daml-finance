module Daml.Package where

import Control.Monad (filterM)
import Data.List (zipWith4)
import qualified Daml.Yaml as Daml (Config(..), damlConfigFile, getDamlConfig)
import qualified Package.Yaml as Package (Config(..), Local, getlocalPackages, path)
import System.FilePath ((</>), splitDirectories)
import System.Directory (doesDirectoryExist, listDirectory, makeAbsolute, makeRelativeToCurrentDirectory)

type T = Local

-- | Represents an individual local build package with its daml config
data Local = Local {
    packageConfig :: Package.Local
  , damlConfig :: Daml.Config
} deriving (Eq, Show)

-- | For a provided package config, extract the daml config per local package
getLocalPackages :: Package.Config -> FilePath -> IO [Local]
getLocalPackages config packageRoot = mapM (`getLocalPackage` packageRoot) $ Package.getlocalPackages config

-- | For a given root file path, extract the necessary package information.
getLocalPackage :: Package.Local -> FilePath -> IO Local
getLocalPackage localPackage packageRoot = do
  path <- makeAbsolute (packageRoot </> Package.path localPackage)
  damlConfig <- Daml.getDamlConfig $ path </> Daml.damlConfigFile
  pure $ Local localPackage damlConfig
