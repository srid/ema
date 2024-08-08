
# Dynamic Model

In [[02-model]], we modified our mood tracker to display the moods from a CSV file. Here, we improve it so that any user modifications to the `data/moods.csv` file will [[hot-reload|hot reload]] the [[live-server]] view of our app in the same manner as [Emanote](https://emanote.srid.ca/) does.

## Dynamic

To do this, we must understand what a [[dynamic]] (which `siteInput` returns) is.

`siteInput` is defined to return a `Dynamic m (RouteModel r)`. In our case, `r ~ Route` and `RouteModel Route ~ Model`, thus our `siteInput` returns a `Dynamic m Model` in the IO monad. A [[dynamic]] is simply defined as:

```haskell
newtype Dynamic m a
  = Dynamic
      ( -- Initial value
        a
      , -- Set a new value
        (a -> m ()) -> m ()
      )
```

It is a pair of values: the initial value and a function that knows how to update that value over time using the user-provided update function (`a -> m ()`). Dynamic's are an `Applicative`, so they compose using `liftA*` family of functions. 

In our `siteInput`, so far, we return `pure myModel`---it has an initial value but does *not* update over time. In order to return an actually updating `Dynamic` of that model, we would change it to: `Dynamic (myModel, updater)` and now the task becomes to define the "updater" function itself. Spelled out:

```haskell
siteInput _ _ = do 
  let myModel = ...
  pure $ Dynamic $ (myModel, ) $ \setModel -> do 
    let loop = do 
          let theNewModel = <some func that returns the next update>
          setModel theNewModel
          loop 
    loop
```

## FSNotify
In the case of our mood tracker, we will use the [fsnotify](https://hackage.haskell.org/package/fsnotify) package (see [[unionmount]] for another option) to fulfill that `<some func that returns the next update>` part. So, without further ado, here's the full implementation of the new `siteInput` that produces a fully-fledged `Dynamic m Model`:

```haskell
  siteInput _ _ = do
    model0 <- readModel "data/moods.csv"
    -- Create a `Dynamic` with initial value (model0) and an updater function
    pure $ Dynamic $ (model0,) $ \setModel -> do
      ch <- liftIO $ watchDirForked "data"
      let loop = do
            logInfoNS "fsnotify" "Waiting for fs event ..."
            evt <- liftIO $ readChan ch
            logInfoNS "fsnotify" $ "Got fs event: " <> show evt
            handle (\(e :: IOException) -> logErrorNS "fsnotify" (show e)) $
              setModel =<< readModel (FSNotify.eventPath evt)
            loop
      loop
    where
      readModel fp = do
        s <- readFileLBS fp
        case toList <$> Csv.decode Csv.NoHeader s of
          Left err -> logErrorNS "csv" (toText err) >> pure (Model mempty)
          Right moods -> pure $ Model $ Map.fromList moods
      -- Observe changes to a directory path, and return the `Chan` of its events.
      watchDirForked :: FilePath -> IO (Chan FSNotify.Event)
      watchDirForked path = do
        ch <- newChan
        void . forkIO $
          FSNotify.withManager $ \mgr -> do
            _stopListening <- FSNotify.watchDirChan mgr path (const True) ch
            threadDelay maxBound
        pure ch
```

Now if you run the app and modify the `data/mood.csv` file (e.g., change "Neutral" to "Bad"), your app's web view will update in real-time. Your Ema app updates instantly on code *or* data change.

This concludes the tutorial series, and hopefully, you have gained an introductory understanding of what is entailed behind the *"just about any app that creates a browser view of arbitrarily changing data"* claim on the [[index|index]] page. You can view the source code for the mood tracker tutorial at  https://github.com/srid/MoodTracker-Tutorial.

You may visit [[guide]] or [[topics]] to further your understanding.
