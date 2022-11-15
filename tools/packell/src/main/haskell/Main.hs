-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Daml.DataDependencies (update, validate, dryRun)
import Daml.Package (getLocalPackages)
import Options.Arguments (Arguments(..), Command(..), DataDependenciesCommand(..), parseInputs)
import Package.Yaml (readPackageYaml)
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory)
import Git.Commands (fetch, hasDiff)

-- | Entry to the Packell application.
main :: IO ()
main = parseInputs >>= run

-- | Runs the specified command based of the passed arguments.
run :: Arguments -> IO ()
run Arguments{optPackageConfigPath, optCommand} = do
  packageConfigPath <- makeAbsolute optPackageConfigPath
  let packageRoot = takeDirectory optPackageConfigPath
  packageYaml <- readPackageYaml packageConfigPath
  packages <- getLocalPackages packageYaml packageRoot

  -- res <- hasDiff "Daml.Finance.Interface.Instrument.Generic/0.1.7" ["/Users/brianweir/git/daml-finance/package/main/daml/ContingentClaims.Lifecycle/daml/ContingentClaims/Lifecycle/Lifecycle.daml"]
  -- print res

  case optCommand of
    DataDependencies command -> case command of
      Update -> update packageRoot packageYaml packages
      Validate -> validate packageRoot packageYaml packages
      DryRun -> dryRun packageRoot packageYaml packages
    Info -> print "Congratulations, you have called a placeholder for future commands!"
