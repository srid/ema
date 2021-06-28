{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Ema.CLI where

import Options.Applicative hiding (action)

data Cli = Cli
  { action :: Action
  }
  deriving (Eq, Show)

data Action
  = Generate FilePath
  | Run
  deriving (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  action <-
    subparser
      (command "gen" (info generate (progDesc "Generate static HTML files")))
      <|> pure Run
  pure Cli {..}
  where
    generate :: Parser Action
    generate =
      Generate <$> argument str (metavar "DEST...")

cliAction :: IO Cli
cliAction = do
  execParser opts
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "Ema - static site generator"
            <> header "Ema"
        )
