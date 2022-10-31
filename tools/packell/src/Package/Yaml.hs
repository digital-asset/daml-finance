{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language DuplicateRecordFields #-}

module Package.Yaml where

import Control.Applicative
import Data.Yaml (FromJSON(parseJSON), (.:), Value(Object), decodeFileEither, ParseException, Parser, withArray, parseJSONList, withObject)
import qualified Data.Vector as V (toList)

type T = Config

-- | Repository details.
data Repo = Repo {
    host :: String
  , organisation :: String
  , name :: String
  } deriving (Eq, Show)

-- | A locally defined package.
data Local = Local {
    path :: String
  , baseModule :: String
  , tagPrefix :: String
  } deriving (Eq, Show)

-- | A wrapper for a local package.
newtype LocalPackage = LocalPackage { localPackage :: Local }
  deriving (Eq, Show)

-- | A wrapper for a local packages.
data LocalPackages = LocalPackages {
    repo :: Repo
  , localPackages :: [LocalPackage]
  } deriving (Eq, Show)

-- | A remotely defined package.
data Remote = Remote {
    name :: String
  , baseModule :: String
  , repo :: Repo
  , tag :: String
  , darName :: String
  } deriving (Eq, Show)

-- | A wrapper for a remote packge.
newtype RemotePackage = RemotePackage { remotePackage :: Remote }
  deriving (Eq, Show)

newtype RemotePackages = RemotePackages { remotePackages :: [RemotePackage] }
  deriving (Eq, Show)

-- | Represents a package config file
data Config = Config {
    installDir :: String
  , remote :: RemotePackages
  , local :: LocalPackages
  } deriving (Eq, Show)

instance FromJSON Repo where
  parseJSON (Object v) =
    Repo <$>
      v .: "host" <*>
      v .: "organisation" <*>
      v .: "name"
  parseJSON _          = error "Cannot parse local from YAML"

instance FromJSON Local where
  parseJSON (Object v) =
    Local <$>
      v .: "path" <*>
      v .: "base-module" <*>
      v .: "tag-prefix"
  parseJSON _          = error "Cannot parse local from YAML"

instance FromJSON LocalPackage where
  parseJSON (Object v) = LocalPackage <$> v .: "package"
  parseJSON _          = error "Cannot parse local package from YAML"

instance FromJSON LocalPackages where
  parseJSON (Object v) =
    LocalPackages <$>
      v .: "repo" <*>
      v .: "packages"
  parseJSON _          = error "Cannot parse local package from YAML"

instance FromJSON Remote where
  parseJSON (Object v) =
    Remote <$>
      v .: "name" <*>
      v .: "base-module" <*>
      v .: "repo" <*>
      v .: "tag" <*>
      v .: "dar-name"
  parseJSON _ = error "Cannot parse remote from YAML"

instance FromJSON RemotePackage where
  parseJSON (Object v) = RemotePackage <$> v .: "package"
  parseJSON _          = error "Cannot parse remote package from YAML"

instance FromJSON RemotePackages where
  parseJSON (Object v) = RemotePackages <$> v .: "packages"
  parseJSON _          = error "Cannot parse remote package from YAML"

instance FromJSON Config where
  parseJSON (Object v) = do
    Config <$>
      v .: "package-install-dir" <*>
      v .: "remote" <*>
      v .: "local"
  parseJSON _          = error "Cannot parse daml config from YAML"

-- | Given a file path to the package config file, this will extract and return the contents.
getPackageYaml :: FilePath -> IO Config
getPackageYaml filePath = do
  packageConfig :: (Either ParseException Config) <- decodeFileEither filePath
  case packageConfig of
    Left exception -> error (show exception)
    Right dc -> pure dc

-- | Extract the remote packages from a package config.
getRemotePackages :: Config -> [Remote]
getRemotePackages = map remotePackage . remotePackages . remote

-- | Extract the local packages from a package config.
getlocalPackages :: Config -> [Local]
getlocalPackages = map localPackage . localPackages . local

-- | Extract base module from a local package.
-- Required as 'baseModule' isnt exported due to a name clash
--  > consider putting local configs into its own module
getLocalBaseModule :: Local -> String
getLocalBaseModule = baseModule

-- | Extract base module from a remote package
-- Required as 'baseModule' isnt exported due to a name clash
--  > consider putting remote configs into its own module
getRemoteBaseModule :: Remote -> String
getRemoteBaseModule = baseModule
