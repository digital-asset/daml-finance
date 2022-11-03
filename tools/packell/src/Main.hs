-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Parameters (Arguments(..), parseInputs)
import System.FilePath
import System.Directory
import System.Directory.Internal.Prelude (getArgs)
import Control.Monad (filterM)
import Daml.Package (getLocalPackages)
import Package.Yaml (readPackageYaml)
import Daml.Import (printImportsToUpdate, updateImports, validateImports)

main :: IO ()
main = parseInputs >>= run

run :: Arguments -> IO ()
run arguments@Arguments{optPackageConfigPath, optCommand} = do
  print optCommand

  -- packageConfigPath <- makeAbsolute optPackageConfigPath
  -- let packageRoot = takeDirectory packageConfigPath
  -- packageYaml <- readPackageYaml packageConfigPath
  -- packages <- getLocalPackages packageYaml packageRoot

  -- updateImports packageRoot packageYaml packages
  -- printImportsToUpdate packageRoot packageYaml packages
  -- validateImports packageRoot packageYaml packages
