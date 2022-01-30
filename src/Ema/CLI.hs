{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ema.CLI where

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Default (Default (def))
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Some (Some (..))
import Ema.Server (Host, Port)
import Options.Applicative hiding (action)

-- | CLI subcommand
data Action res where
  Generate :: FilePath -> Action [FilePath]
  Run :: (Host, Port) -> Action ()

$(deriveGEq ''Action)
$(deriveGShow ''Action)
$(deriveGCompare ''Action)
$(deriveArgDict ''Action)

isLiveServer :: Some Action -> Bool
isLiveServer (Some (Run _)) = True
isLiveServer _ = False

data Cli = Cli
  { action :: (Some Action)
  }
  deriving stock (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  action <-
    subparser
      (command "gen" (info generate (progDesc "Generate static HTML files")))
      <|> subparser (command "run" (info run (progDesc "Run the live server")))
      <|> pure (Some $ Run def)
  pure Cli {..}
  where
    run :: Parser (Some Action)
    run =
      Some . Run
        <$> ( (,) <$> strOption (long "host" <> short 'h' <> metavar "HOST" <> help "Host to bind to" <> value def)
                <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Port to bind to" <> value def)
            )
    generate :: Parser (Some Action)
    generate =
      Some . Generate <$> argument str (metavar "DEST...")

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
