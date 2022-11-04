-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Options.Arguments (Arguments(..), Command(..), DataDependenciesCommand(..), parseInputs)
import System.FilePath (takeDirectory)
import System.Directory (makeAbsolute)
import Daml.Package (getLocalPackages)
import Package.Yaml (readPackageYaml)
import Daml.DataDependencies (update, validate, dryRun)

main :: IO ()
main = parseInputs >>= run

run :: Arguments -> IO ()
run Arguments{optPackageConfigPath, optCommand} = do
  packageConfigPath <- makeAbsolute optPackageConfigPath
  let packageRoot = takeDirectory optPackageConfigPath
  packageYaml <- readPackageYaml packageConfigPath
  packages <- getLocalPackages packageYaml packageRoot

  case optCommand of
    DataDependencies command -> case command of
      Update -> update packageRoot packageYaml packages
      Validate -> validate packageRoot packageYaml packages
      DryRun -> dryRun packageRoot packageYaml packages
    Info -> print "Congratulations, you have called a placeholder for future commands!"
