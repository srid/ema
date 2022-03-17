{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Ema.App.Env where

import Chronos qualified as C
import Colog
import Control.Concurrent (ThreadId)
import Data.TypeRepMap qualified as TM
import Ema.CLI (Cli)
import UnliftIO (MonadUnliftIO)

data Env m = Env
  { envCli :: Cli,
    envLogAction :: LogAction m Message
  }

instance HasLog (Env m) Message m where
  getLogAction = envLogAction
  setLogAction x env = env {envLogAction = x}

newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (Env App), MonadUnliftIO)

runApp :: Env App -> App a -> IO a
runApp env app = runReaderT (unApp app) env

type instance FieldType "ema" = Text

emaFieldMap :: MonadIO m => FieldMap m
emaFieldMap =
  fromList
    [ #ema (pure ("ema!" :: Text))
    ]

mkEnv :: Cli -> Env App
mkEnv cli = Env @App cli mkMessageAction

mkMessageAction :: LogAction App Message
mkMessageAction =
  upgradeMessageAction (defaultFieldMap <> emaFieldMap) $
    cmapM (fmap encodeUtf8 . fmtRichMessageDefault') logByteStringStdout

fmtRichMessageDefault' :: MonadIO m => RichMessage m -> m Text
fmtRichMessageDefault' RichMsg {..} = do
  maybeThreadId <- extractField $ TM.lookup @"threadId" richMsgMap
  maybePosixTime <- extractField $ TM.lookup @"posixTime" richMsgMap
  mEma <- extractField $ TM.lookup @"ema" richMsgMap
  pure $ formatRichMessage maybeThreadId maybePosixTime mEma richMsgMsg
  where
    formatRichMessage :: Maybe ThreadId -> Maybe C.Time -> Maybe Text -> Message -> Text
    formatRichMessage (maybe "" showThreadId -> thread) (maybe "" showTime -> time) (fromMaybe "NOEMA" -> ema) Msg {..} =
      showSeverity msgSeverity
        <> time
        <> ema
        <> showSourceLoc msgStack
        <> thread
        <> msgText

showThreadId :: ThreadId -> Text
showThreadId = square . show @Text

showTime :: C.Time -> Text
showTime t =
  -- Just showing hh:mm. But we probably should display full datetime if user
  -- passes --verbose.
  -- FIXME: local time, not UTC.
  let C.TimeOfDay h m _ = C.datetimeTime $ C.timeToDatetime t
   in -- FIXME: Use chronos formatter.
      square $ show h <> ":" <> show m

square :: Text -> Text
square t = "[" <> t <> "] "