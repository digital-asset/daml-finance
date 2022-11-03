-- Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Parameters (
    Arguments(..)
  , parameters
  , parseInputs
) where

import Options.Applicative(
    (<**>)
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

-- https://github.com/pcapriotti/optparse-applicative

-- | Main set of commands.
data Command
    = Dependencies DependenciesCommand
    | Info
  deriving (Show)

-- | Commands for imports.
data DependenciesCommand
    = Update
    | Validate
    | DryRun
  deriving (Show)

-- | Input variables to the application
data Arguments = Arguments {
    optPackageConfigPath :: FilePath
  , optCommand :: Command
  } deriving (Show)

dependencyParser :: Parser Command
dependencyParser =
  Dependencies
    <$> hsubparser (command "update" (info (pure Update) (progDesc "Update package data-dependencies based of package imports."))
      <> command "validate" (info (pure Validate) (progDesc "Validate if any data-dependencies require updating."))
      <> command "dryrun" (info (pure DryRun) (progDesc "Print out any data-dependencies that require updating.")))

dependenciesCommand :: Mod CommandFields Command
dependenciesCommand = command "dependencies" (info dependencyParser (progDesc "Update package data-dependencies based on package imports."))

infoCommand :: Mod CommandFields Command
infoCommand = command "info" (info (pure Info) (progDesc "Update package data-dependencies based on package imports."))

-- | Input Parameters to the application
parameters :: Parser Arguments
parameters =
  Arguments
    <$> strOption (long "package"
      <> short 'p'
      <> metavar "CONFIG"
      <> showDefault
      <> value "package/package.yaml"
      <> help "Path to the package configuration")
    <*> hsubparser (dependenciesCommand
      <> metavar "dependencies COMMAND")

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
