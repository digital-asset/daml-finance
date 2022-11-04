-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Options.Arguments (
    Arguments(..)
  , Command(..)
  , DataDependenciesCommand(..)
  , parameters
  , parseInputs
) where

import Options.Applicative(
    (<**>)
  , (<|>)
  , Parser
  , auto
  , fullDesc
  , header
  , help
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  , execParser
  , helper
  , showHelpOnError
  , ParserInfo
  , customExecParser
  , prefs, hsubparser, command, strArgument, Alternative (many), Mod, CommandFields, commandGroup)

-- | Packell main commands set.
data Command
    = DataDependencies DataDependenciesCommand
    | Info
  deriving (Show)

-- | Commands for working with dependencies.
data DataDependenciesCommand
    = Update
    | Validate
    | DryRun
  deriving (Show)

-- | Input arguments to the application.
data Arguments = Arguments {
    optPackageConfigPath :: FilePath
  , optCommand :: Command
  } deriving (Show)

-- | The main command parser.
commandParser :: Parser Command
commandParser =
  hsubparser (
    command "data-dependencies" (info dataDependenciesParser (progDesc "Update package data-dependencies based on package sources."))
      <> metavar "data-dependencies COMMAND")
  <|> hsubparser (
    command "info" (info (pure Info) (progDesc "Test command - a stub for future commands."))
      <> metavar "info")

-- | Command parser for dependencies.
dataDependenciesParser :: Parser Command
dataDependenciesParser =
  DataDependencies
    <$> hsubparser (command "update" (info (pure Update) (progDesc "Update package data-dependencies based on imports in package sources."))
      <> command "validate" (info (pure Validate) (progDesc "Validate if package data-dependencies require updating. Throws an exception if any updates are found. This does not update any data-dependencies."))
      <> command "dryrun" (info (pure DryRun) (progDesc "Displays all package data-dependencies that require updating. This does not update any data-dependencies.")))

-- | Input Parameters to the application.
parameters :: Parser Arguments
parameters =
  Arguments
    <$> strOption (long "package"
      <> short 'p'
      <> metavar "CONFIG"
      <> showDefault
      <> value "package/package.yaml"
      <> help "Path to the package configuration")
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
