{-# LANGUAGE AllowAmbiguousTypes #-}

module Ema.Site (
  EmaSite (..),
) where

import Control.Monad.Logger (MonadLoggerIO)
import Data.Some (Some)
import Ema.Asset (Asset)
import Ema.CLI qualified as CLI
import Ema.Dynamic (Dynamic)
import Ema.Route.Class (IsRoute (RouteModel))
import Ema.Route.Encoder (RouteEncoder)
import Optics.Core (Prism')
import UnliftIO (MonadUnliftIO)

{- | Typeclass to orchestrate an Ema site

  Given a route `r` from the class of `IsRoute` types, instantiating EmaSite
  on it enables defining the static site pipeline as follows:

  @
  SiteArg -> siteInput -> Dynamic model --[r, model]--> siteOutput
  @

  - `SiteArg` is typically not used, but it can be used to pass command line
  arguments and other such settings.
  - `siteInput` returns a time-varying value (Dynamic) representing the data for
  your static site.
  - `siteOutput` takes this data model (oneshot value) and returns the generated
  content (usually HTML) for the given route.

  Finally, `Ema.App.runSite @r arg` (where `arg` is of type `SiteArg`) is run
  from the `main` entry point to run your Ema site.
-}
class IsRoute r => EmaSite r where
  {- `SiteArg` is typically settings from the environment (config file, or
    command-line arguments) that your Dynamic-producing `siteInput` function
    consumes as argument.
  -}
  type SiteArg r :: Type

  -- By default Ema sites have no site arguments.
  type SiteArg r = ()

  {- Get the model's time-varying value as a `Dynamic`.

    If your model is not time-varying, use `pure` to produce a constant value.

    The default implementation works with generic deriving of `IsRoute`.
  -}
  siteInput ::
    forall m.
    (MonadIO m, MonadUnliftIO m, MonadLoggerIO m) =>
    Some CLI.Action ->
    -- | The value passed by the programmer to `Ema.App.runSite`
    SiteArg r ->
    -- | Time-varying value of the model. If your model is not time-varying, use
    -- `pure` to produce a constant value.
    m (Dynamic m (RouteModel r))

  -- | Return the generated asset for the given route and model.
  siteOutput :: Prism' FilePath r -> RouteModel r -> r -> Asset LByteString
