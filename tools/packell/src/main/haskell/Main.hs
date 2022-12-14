-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (when)
import qualified Daml.DataDependencies as D (update, validate, dryRun)
import Daml.Package (getLocalPackages)
import Options.Arguments (Arguments(..), Command(..), parseInputs)
import qualified Options.DataDependencies as D (DataDependenciesCommand(..))
import qualified Options.Versioning as V (VersioningCommand(..))
import Package.Yaml (readPackageYaml)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
import qualified Git.Commands as Git (fetch)
import Spin.Functions (withSpinner)
import Spin.Specs (dotsSpec)
import Spin.Types (Config(configMessage))

-- | Entry to the Packell application.
main :: IO ()
main = parseInputs >>= run

-- | Runs the specified command based of the passed arguments.
run :: Arguments -> IO ()
run Arguments{optPackageConfigPath, optFetch, optCommand} = do
  packageConfigPath <- makeAbsolute optPackageConfigPath
  let packageRoot = takeDirectory optPackageConfigPath
  packageYaml <- readPackageYaml packageConfigPath
  packages <- getLocalPackages packageYaml packageRoot

  when optFetch $ do
    withSpinner dotsSpec $ \up -> do
      up $ \c -> c {
        configMessage = "Running 'git fetch'..."
      }
      Git.fetch
    putStrLn "Running 'git fetch'... successfully completed!"

  case optCommand of
    DataDependencies command -> case command of
      D.Update -> D.update packageRoot packageYaml packages
      D.Validate -> D.validate packageRoot packageYaml packages
      D.DryRun -> D.dryRun packageRoot packageYaml packages
    Versioning command -> case command of
      V.BumpAll -> putStrLn "BumpAll"
      V.Update -> putStrLn "Update"
      V.Validate -> putStrLn "Validate"
      V.DryRun -> putStrLn "DryRun"
