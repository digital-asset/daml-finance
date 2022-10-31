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
import Daml.Source (processPackages)
import Package.Yaml (getPackageYaml)

main :: IO ()
main = parseInputs >>= run

run :: Arguments -> IO ()
run arguments@Arguments{packageConfigPath} = do
  packageConfigPath <- makeAbsolute packageConfigPath
  let packageRoot = takeDirectory packageConfigPath
  packageYaml <- getPackageYaml packageConfigPath
  -- print packageYaml
  packages <- getLocalPackages packageYaml packageRoot
  -- print packages
  processPackages packageRoot packageYaml packages
