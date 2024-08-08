
# Add a Model

To generate our mood tracker view, we need ... mood data, i.e., the mood [[model]]. If we are recording our mood each day, then Haskell's `Map` type is one way to represent moods over time.

```haskell
data Model = Model 
  { modelDays :: Map Date Mood 
  }

data Mood = Bad | Neutral | Good
  deriving stock (Show, Read)
```

Now we want to *associate* our `Route` type from [[01-routes]] with this `Model`. This can be done as follows:

1. When [[generic|genericaly deriving]] routes, use the `WithModel` option to associate a model for that route. 
2. Use the same[^same] model in the `IsRoute` instance for subroutes (here, `Date`). 
3. Change `EmaSite`'s `siteInput` method to return the model; and `siteOutput` to use the new model

[^same]: Subroutes can of course use a different model. The `WithSubModels` option can be used to control this.

To achieve (1), we would change the deriving clause for our `Route` to the following:

```haskell
deriveGeneric ''Route
deriveIsRoute ''Route [t|'[ WithModel Model ]|]
```

To achieve (2):

```haskell
instance IsRoute Date where
  type RouteModel Date = Model -- ^ We changed `()` to `Model`
  routePrism (Model _moods) = toPrism_ $
    prism'
      ( \(Date (y, m, d)) ->
          formatTime defaultTimeLocale "%Y-%m-%d.html" $
            fromGregorian y m d
      )
      ( fmap (Date . toGregorian)
          . parseTimeM False defaultTimeLocale "%Y-%m-%d.html"
      )
  routeUniverse (Model moods) = Map.keys moods -- ^ We implemented this
```

Notice how this time we are able to properly define `routeUniverse` (it is used during static site generation, to determine which routes to generate on disk), because the model value is available. `routePrism` also gets the model as an argument, but in this case we have no need for it (in theory, we could check that a date exists before decoding successfully).

Finally, (3) is where we get to produce (`siteInput`) and consume (`siteOutput`) the model when rendering the site. The next section explains this in detail.
## Use `Model`

We are yet to *use* our model to do anything meaningful. The most meaningful thing to do here is to render HTML for our routes. Change the `siteOutput` to following (we use blaze-html library):

```haskell
instance EmaSite Route where
  siteInput _ _ = pure $ pure $ Model mempty -- Empty model for now
  siteOutput rp model r =
    pure . Ema.AssetGenerated Ema.Html . RU.renderHtml $ do
      H.docType
      H.html ! A.lang "en" $ do
        H.head $ do
          H.title "Mood tracker"
        H.body $ case r of
          Route_Index -> do
            H.h1 "Mood tracker"
            -- Just list the moods
            forM_ (Map.toList $ modelDays model) $ \(date, mood) -> do
              H.li $ do
                let url = Ema.routeUrl rp $ Route_Date date
                H.a ! A.href (H.toValue url) $ show date
                ": "
                show mood
          Route_Date d -> do
            H.h1 (show d)
            H.pre $ show $ Map.lookup d (modelDays model)
```

This should render both `/` (`Route_Index`) and, say, `/date/2020-01-01.html` (`Route_Date ...`) in your browser. However, it won't have any moods since our `Model` is empty per the `siteInput` definition! Let's fix that.

## Represent `Model` using CSV

Ultimately the value for our `Model` will come from elsewhere, such as a CSV file on disk.  Let's use [cassava](https://hackage.haskell.org/package/cassava) to parse this CSV and load it into our Model.

First, add a sample CSV file under `./data/moods.csv` containing:

```csv
2022-04-23,Good
2022-04-24,Neutral
```

Now change the `siteInput` function to replace `mempty` with the contents of this CSV file loaded as `Model`:

```haskell
import Data.Csv qualified as Csv

instance EmaSite Route where
  siteInput _ _ = do
    s <- readFileLBS "data/moods.csv"
    case toList <$> Csv.decode Csv.NoHeader s of
      Left err -> throw $ userError err
      Right moods ->
        pure $ pure $ Model $ Map.fromList moods
```

Note that this will require that you define cassava's `FromField` instances on `Date` and `Mood` types. A simple implementation is provided below:

```haskell
instance Csv.FromField Date where
  parseField f = do
    s <- Csv.parseField @String f
    case parseTimeM False defaultTimeLocale "%Y-%m-%d" s of
      Left err -> fail err
      Right date ->
        pure $ Date $ toGregorian date

instance Csv.FromField Mood where
  parseField f = do
    s <- Csv.parseField @String f
    case readEither @Mood s of
      Left err -> fail $ toString err
      Right v -> pure v
```

The result is that our site's index page will display the moods in the CSV file, along with the link to the particular day routes (`Route_Date`). 

This is great so far---we can track how we feel in `moods.csv` and get an app-like "view" of it. But, we don't have [[hot-reload]]. Changing `data/moods.csv` ought to update our site. The final step of our tutorial series will explain this.

{.last}
[Next]{.next}, [[03-dynamic|we will enable]] hot-reload on the mood model.
