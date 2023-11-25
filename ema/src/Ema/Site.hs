{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.Site (
  EmaSite (..),
  EmaStaticSite,
) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Some (Some)
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic)
import Ema.Route.Class (IsRoute (RouteModel))
import Optics.Core (Prism')
import UnliftIO (MonadUnliftIO)

{- | Typeclass to orchestrate an Ema site

  Given a route `r` from the class of `IsRoute` types, instantiating EmaSite
  on it enables defining the site build pipeline as follows:

  @
  SiteArg -> siteInput -> Dynamic model --[r, model]--> siteOutput
  @

  - `SiteArg` is typically not used, but it can be used to pass command line
  arguments and other such settings.
  - `siteInput` returns a time-varying value (Dynamic) representing the data for
  your static site.
  - `siteOutput` takes this data model (oneshot value) and returns the generated
  content (usually HTML asset, per `SiteOutput`) for the given route.

  Finally, `Ema.App.runSite @r arg` (where `arg` is of type `SiteArg`) is run
  from the `main` entry point to run your Ema site.
-}
class (IsRoute r) => EmaSite r where
  -- | `SiteArg` is typically settings from the environment (config file, or
  --    command-line arguments) that your Dynamic-producing `siteInput` function
  --    consumes as argument.
  type SiteArg r :: Type

  -- By default Ema sites have no site arguments.
  type SiteArg r = ()

  -- | Type of the value returned by `siteOutput`. Usually `Asset LByteString`
  -- but it can be anything.
  type SiteOutput r :: Type

  type SiteOutput r = Asset LByteString

  -- | Get the model's time-varying value as a `Dynamic`.
  --
  --    If your model is not time-varying, use `pure` to produce a constant value.
  siteInput ::
    forall m.
    (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
    Some CLI.Action ->
    -- | The value passed by the programmer to `Ema.App.runSite`
    SiteArg r ->
    -- | Time-varying value of the model. If your model is not time-varying, use
    -- `pure` to produce a constant value.
    m (Dynamic m (RouteModel r))

  -- | Return the output (typically an `Asset`) for the given route and model.
  siteOutput ::
    forall m.
    (MonadIO m, MonadLoggerIO m) =>
    Prism' FilePath r ->
    RouteModel r ->
    r ->
    m (SiteOutput r)

-- | Like `EmaSite` but `SiteOutput` is a bytestring `Asset`.
type EmaStaticSite r = (EmaSite r, SiteOutput r ~ Asset LByteString)
