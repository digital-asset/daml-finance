module Parameters where

-- https://github.com/pcapriotti/optparse-applicative

import Options.Applicative((<**>)
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
        , Parser )

-- | Input variables to the application
data Arguments = Arguments {
    packageConfigPath :: FilePath
  } deriving (Show)

-- | Input Parameters to the application
parameters :: Parser Arguments
parameters =
  Arguments
    <$> strOption (long "package"
      <> short 'p'
      <> metavar "CONFIG"
      <> showDefault
      <> value "package/package.yaml"
      <> help "Path to the package configuration"
    )

-- | Parse application arguments
parseInputs :: IO Arguments
parseInputs = execParser opts
  where
    opts = info (parameters <**> helper)
      ( fullDesc
     <> progDesc "Process Daml-Finance packages"
     <> header "Packell - a Daml-Finance package helper")
