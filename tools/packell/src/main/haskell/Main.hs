-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (when)
import Daml.DataDependencies (update, validate, dryRun)
import Daml.Package (getLocalPackages)
import Options.Arguments (Arguments(..), Command(..), DataDependenciesCommand(..), parseInputs)
import Package.Yaml (readPackageYaml, IncrementVersion (MAJOR, PATCH, MINOR, SNAPSHOT))
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
import qualified Git.Commands as Git (fetch, hasDiff)
import Spin.Functions (withSpinner)
import Spin.Specs (dotsSpec)
import Spin.Types (Config(configMessage))
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)

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
      Update -> update packageRoot packageYaml packages
      Validate -> validate packageRoot packageYaml packages
      DryRun -> dryRun packageRoot packageYaml packages
    Info -> putStrLn "Congratulations, you have called a placeholder for future commands!"
