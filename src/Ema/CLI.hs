{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ema.CLI where

import Control.Monad.Logger (LogLevel (LevelDebug, LevelInfo), MonadLoggerIO, logErrorN)
import Control.Monad.Logger.Extras (
  Logger (Logger),
  colorize,
  logToStdout,
 )
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Default (Default (def))
import Data.GADT.Compare.TH (
  DeriveGCompare (deriveGCompare),
  DeriveGEQ (deriveGEq),
 )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Some (Some (..))
import Network.Wai.Handler.Warp (Port)
import Options.Applicative hiding (action)

-- | Host string to start the server on.
newtype Host = Host {unHost :: Text}
  deriving newtype (Eq, Show, Ord, IsString)

instance Default Host where
  def = "127.0.0.1"

-- | CLI subcommand
data Action res where
  Generate :: FilePath -> Action [FilePath]
  Run :: (Host, Maybe Port) -> Action ()

$(deriveGEq ''Action)
$(deriveGShow ''Action)
$(deriveGCompare ''Action)
$(deriveArgDict ''Action)

isLiveServer :: Some Action -> Bool
isLiveServer (Some (Run _)) = True
isLiveServer _ = False

data Cli = Cli
  { action :: Some Action
  , verbose :: Bool
  }
  deriving stock (Eq, Show)

cliParser :: Parser Cli
cliParser = do
  action <-
    subparser
      (command "gen" (info generate (progDesc "Generate static site")))
      <|> subparser (command "run" (info run (progDesc "Run the live server")))
      <|> pure (Some $ Run def)
  verbose <- switch (long "verbose" <> short 'v' <> help "Enable verbose logging")
  pure Cli {..}
  where
    run :: Parser (Some Action)
    run =
      Some . Run <$> hostPortParser
    generate :: Parser (Some Action)
    generate =
      Some . Generate <$> argument str (metavar "DEST...")

hostPortParser :: Parser (Host, Maybe Port)
hostPortParser =
  (,) <$> hostParser <*> optional portParser

hostParser :: Parser Host
hostParser =
  strOption (long "host" <> short 'h' <> metavar "HOST" <> help "Host to bind to" <> value def)

portParser :: Parser Port
portParser =
  option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Port to bind to")

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

getLogger :: Cli -> Logger
getLogger cli =
  logToStdout
    & colorize
    & allowLogLevelFrom (bool LevelInfo LevelDebug $ verbose cli)
  where
    allowLogLevelFrom :: LogLevel -> Logger -> Logger
    allowLogLevelFrom minLevel (Logger f) = Logger $ \loc src level msg ->
      if level >= minLevel
        then f loc src level msg
        else pass

{- | Crash the program with the given error message

 First log the message using Error level, and then exit using `fail`.
-}
crash :: (MonadLoggerIO m, MonadFail m) => Text -> m a
crash msg = do
  logErrorN msg
  fail $ toString msg