{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Ema.CLI where

import Control.Monad.Logger (LogLevel (LevelDebug, LevelInfo), LogSource, MonadLoggerIO, logErrorNS)
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
data Action result where
  -- | Generate static files at the given output directory, returning the list
  -- of generated files.
  Generate :: FilePath -> Action [FilePath]
  -- | Run the live server
  Run :: (Host, Maybe Port) -> Action ()

$(deriveGEq ''Action)
$(deriveGShow ''Action)
$(deriveGCompare ''Action)
$(deriveArgDict ''Action)

isLiveServer :: Some Action -> Bool
isLiveServer (Some (Run _)) = True
isLiveServer _ = False

-- | Ema's command-line interface options
data Cli = Cli
  { action :: Some Action
  -- ^ The Ema action to run
  , verbose :: Bool
  -- ^ Logging verbosity
  }
  deriving stock (Eq, Show)

instance Default Cli where
  -- By default, run the live server on random port.
  def = Cli (Some (Run def)) False

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
      fmap (Some . Run) $ (,) <$> hostParser <*> optional portParser
    generate :: Parser (Some Action)
    generate =
      Some . Generate <$> argument str (metavar "DEST")

hostParser :: Parser Host
hostParser =
  strOption (long "host" <> short 'h' <> metavar "HOST" <> help "Host to bind to" <> value def)

portParser :: Parser Port
portParser =
  option auto (long "port" <> short 'p' <> metavar "PORT" <> help "Port to bind to")

-- | Parse Ema CLI arguments passed by the user.
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
crash :: (MonadLoggerIO m, MonadFail m) => LogSource -> Text -> m a
crash source msg = do
  logErrorNS source msg
  fail $ toString msg
