---
order: 4
---

# Dynamic


`Dynamic`'s are essential to support [[hot-reload]].

A `Dynamic` is simply defined as:

```haskell
newtype Dynamic m a
  = Dynamic
      ( -- Initial value
        a
      , -- Set a new value
        (a -> m ()) -> m ()
      )
```

It is a pair of values: the initial value, and a function that knows how to update that value over time using the user-provided update function (`a -> m ()`). Dynamic's are an `Applicative`, so they compose using `liftA*` family of functions. 

The [[site]]'s `siteInput` method returns a `Dynamic` of [[model]], which represents all the data required to render a site. If you do not want [[hot-reload]], you may return a `pure` value.

The use of a time-varying `Dynamic` is what enables [[hot-reload]]. See [here](https://github.com/fpindia/fpindia-site/pull/24/files) for an example of making a model time-varying. Checkout [[unionmount]] to produce a `Dynamic` of a model that updates based on the filesystem tree.

## Reading the current value from outside

`Dynamic` is push-only: its updater hands each new value to a callback the consumer supplies. That shape fits Ema's render loop, but not a second component that wants synchronous pull access to the latest value — e.g. an HTTP handler running alongside `runSiteWith` that needs the current model per request.

`Ema.Dynamic.currentValue` tees a `Dynamic` for that case:

```haskell
currentValue :: MonadIO m => Dynamic m a -> m (IO a, Dynamic m a)
```

It returns a reader (`IO a`, yielding the most recently pushed value, or the initial value before any update) and a pass-through `Dynamic` that must be used in place of the input — the pass-through's updater is wired to feed the reader on each update.

```haskell
(readModel, dyn') <- currentValue dyn
race_
  (runSiteWith cfg arg dyn')          -- consumes dyn'
  (serve $ \_req -> readModel)        -- reads the live model per request
```

Only one producer runs — the pass-through intercepts the send callback, it does not duplicate the underlying updater.

`Ema.App.runSiteWithInput` takes a pre-built `Dynamic` instead of calling `siteInput` for you, which is what makes the `race_ (…) (runSiteWith …)` shape workable:

```haskell
flip runLoggerLoggingT logger $ do
  rawDyn          <- siteInput @r action arg       -- your EmaSite.siteInput runs
  (readModel, dyn') <- currentValue rawDyn         -- tee for out-of-band reads
  liftIO $ publishReader readModel                 -- hand the reader to an observer
  withRunInIO $ \runIO -> race_
    (myObserver readModel)                          -- whatever needs the live model
    (runIO $ runSiteWithInput cfg dyn')            -- Ema drives the wrapped Dynamic
```

`runSiteWith` itself becomes a one-liner wrapper around `siteInput` + `runSiteWithInput`, so callers that don't need the tee are unaffected.