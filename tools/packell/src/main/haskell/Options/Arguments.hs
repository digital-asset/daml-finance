-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Options.Arguments (
    Arguments(..)
  , Command(..)
  , parameters
  , parseInputs
) where

import Options.Applicative (
    (<**>)
  , (<|>)
  , Parser
  , ParserInfo
  , auto
  , command
  , customExecParser
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , long
  , metavar
  , option
  , prefs
  , progDesc
  , short
  , showDefault
  , showHelpOnError
  , strOption
  , switch
  , value
  )
import qualified Options.Versioning as V (VersioningCommand(..))
import qualified Options.DataDependencies as D (DataDependenciesCommand(..))

-- | Packell main commands set.
data Command
    = DataDependencies D.DataDependenciesCommand
    | Versioning V.VersioningCommand
  deriving (Show)

-- | Input arguments to the application.
data Arguments = Arguments {
    optPackageConfigPath :: FilePath
  , optFetch :: Bool
  , optCommand :: Command
  } deriving (Show)

-- | Command parser for dependencies.
dataDependenciesParser :: Parser Command
dataDependenciesParser =
  DataDependencies
    <$> hsubparser (command "update" (info (pure D.Update) (progDesc "Update package data-dependencies based on imports in package sources."))
      <> command "validate" (info (pure D.Validate) (progDesc "Validate if package data-dependencies require updating. Throws an exception if any updates are found. This does not update any data-dependencies."))
      <> command "dry-run" (info (pure D.DryRun) (progDesc "Displays all package data-dependencies that require updating. This does not update any data-dependencies.")))

-- | Command parser for versioning.
versioningParser :: Parser Command
versioningParser =
  Versioning
    <$> hsubparser (command "update" (info (pure V.Update) (progDesc "Update package versions based on commits."))
      <> command "validate" (info (pure V.Validate) (progDesc "Validate if package versions require updating based off commits. Throws an exception if any updates are found. This does not update any versions."))
      <> command "dry-run" (info (pure V.DryRun) (progDesc "Displays all package versions that require updating based off commits. This does not update any versions."))
      <> command "bump-all" (info (pure V.BumpAll) (progDesc "Update all package versions which match released packages."))
      <> command "force-bump-all" (info (pure V.ForceBumpAll) (progDesc "Force update on all package versions, regardless if they have been released or not.")))

-- | The main command parser.
commandParser :: Parser Command
commandParser =
  hsubparser (
    command "data-dependencies" (info dataDependenciesParser (progDesc "Update package data-dependencies based on package sources."))
      <> metavar "data-dependencies COMMAND")
  <|> hsubparser (
    command "versioning" (info versioningParser (progDesc "Update package versioning based on commits."))
      <> metavar "versioning COMMAND")

-- | Input Parameters to the application.
parameters :: Parser Arguments
parameters =
  Arguments
    <$> strOption (long "package"
      <> short 'p'
      <> metavar "CONFIG"
      <> showDefault
      <> value "package/packages.yaml"
      <> help "Path to the package configuration")
    <*> switch (long "fetch"
      <> short 'f'
      <> help "Fetch from git")
    <*> commandParser

-- | Custom exex parser to display options, subcommands, help, etc. on error.
showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)

-- | Parse application arguments
parseInputs :: IO Arguments
parseInputs = showHelpOnErrorExecParser opts
  where
    opts = info (parameters <**> helper)
      ( fullDesc
     <> progDesc "Process Daml-Finance packages"
     <> header "Packell - a Daml-Finance package helper")
