# 02 Add a Model

What data do we need to generate our mood tracker website? If we record our mood each day, then Haskell's `Map` type is one way to represent the moods overtime.

```haskell
data Model = Model 
  { modelDays :: Map Date Mood 
  }

data Mood = Bad | Neutral | Good
```

Now we want to associate our `Route` types with this `Model`. In [[generic|generic deriving]] the `WithModel` option can be used to do this. And we must also use the same model in the `IsRoute` instance for subroutes. The specific changes required on top of [[01-routes]] is:

```diff
diff --git a/src/Main.hs b/src/Main.hs
index 2bb65f9..37f2308 100644
--- a/src/Main.hs
+++ b/src/Main.hs
@@ -20,7 +20,8 @@ data Route
     (HasSubRoutes, HasSubModels, IsRoute)
     via ( GenericRoute
             Route
-            '[ -- This is automatically deduced in GHC 9.2
+            '[ WithModel Model
+             , -- This is automatically deduced in GHC 9.2
                -- But nixpkgs is still oin 9.0, so we must manually specify it.
                WithSubRoutes
                 '[ FileRoute "index.html"
@@ -35,8 +36,8 @@ newtype Date = Date (Integer, Int, Int)
     (Show, Eq, Ord, Generic)
 
 instance IsRoute Date where
-  type RouteModel Date = ()
-  routeEncoder = mkRouteEncoder $ \() ->
+  type RouteModel Date = Model
+  routeEncoder = mkRouteEncoder $ \(Model moods) ->
     prism'
       ( \(Date (y, m, d)) ->
           formatTime defaultTimeLocale "%Y-%m-%d.html" $
@@ -47,9 +48,15 @@ instance IsRoute Date where
       )
   allRoutes _ = []
 
+data Model = Model
+  { modelDays :: Map Date Mood
+  }
+
+data Mood = Bad | Neutral | Good
+
 instance EmaSite Route where
-  siteInput _ _ = pure $ pure ()
-  siteOutput rp () r = Ema.AssetGenerated Ema.Html "TODO"
+  siteInput _ _ = pure $ pure $ Model mempty
+  siteOutput rp _model r = Ema.AssetGenerated Ema.Html "TODO"
 
 main :: IO ()
 main = Ema.runSite_ @Route ()
\ No newline at end of file
```

Note that we are not *using* our model yet.

So why not do it now, by rendering a basic HTML of it? Change the `siteOutput` to following (we use blaze-html library):

```haskell
instance EmaSite Route where
  siteInput _ _ = pure $ pure $ Model mempty
  siteOutput rp model r =
    Ema.AssetGenerated Ema.Html . RU.renderHtml $ do
      H.docType
      H.html ! A.lang "en" $ do
        H.head $ do
          H.title "Mood tracker"
        H.body $ case r of
          Route_Index -> do
            H.h1 "Mood tracker"
            forM_ (Map.toList $ modelDays model) $ \(date, mood) -> do
              H.li $ do
                let url = Ema.routeUrl rp $ Route_Date date
                H.a ! A.href (H.toValue url) $
                  show date
                ": "
                show mood
          Route_Date d -> do
            H.h1 (show d)
            H.pre $ show $ Map.lookup d (modelDays model)
```

This should render both `/` (`Route_Index`) and, say, `/date/2020-01-01.html` (`Route_Date ...`) in your browser. However, it won't have any moods since our `Model` is empty per the `siteInput` definition! We will fix this in the next section -- by defining a [[dynamic]] of our `Model`. Finish this tutorial series by reading [[03-dynamic]].