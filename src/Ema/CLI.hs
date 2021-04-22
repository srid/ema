{-# LANGUAGE TypeApplications #-}

module Ema.CLI where

import Options.Applicative

data Action
  = Generate FilePath
  | Run
  deriving (Eq, Show)

actionParser :: Parser Action
actionParser =
  subparser
    (command "gen" (info generate (progDesc "Generate static HTML files")))
    <|> pure Run
  where
    generate :: Parser Action
    generate =
      Generate <$> argument str (metavar "DEST...")

cliAction :: IO Action
cliAction = do
  execParser opts
  where
    opts =
      info
        (actionParser <**> helper)
        ( fullDesc
            <> progDesc "Ema - desc"
            <> header "Ema - header"
        )