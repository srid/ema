{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Ema.CLI where

import Data.Constraint.Extras.TH (deriveArgDict)
import Data.GADT.Compare.TH
  ( DeriveGCompare (deriveGCompare),
    DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Some
import Options.Applicative hiding (action)

data Action res where
  Generate :: FilePath -> Action [FilePath]
  Run :: Action ()

$(deriveGEq ''Action)
$(deriveGShow ''Action)
$(deriveGCompare ''Action)
$(deriveArgDict ''Action)

data Cli = Cli
  { action :: (Some Action)
  }
  deriving (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  action <-
    subparser
      (command "gen" (info generate (progDesc "Generate static HTML files")))
      <|> pure (Some Run)
  pure Cli {..}
  where
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
