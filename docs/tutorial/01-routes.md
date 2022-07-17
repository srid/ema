# 01 Add Routes

We want to write a application to track moods, ie., a [mood tracking](https://en.wikipedia.org/wiki/Mood_tracking) app. 

The first step is to think about the various pages and define their corresponding route types. Our application will have an index page (displaying mood summary) as well as pages specific to the individual days.

```haskell
data Route
  = Route_Index
  | Route_Date Date
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            Route
            '[ -- This is automatically deduced in GHC 9.2
               -- But nixpkgs is still on 9.0, so we must 
               -- manually specify it.
               WithSubRoutes
                '[ FileRoute "index.html"
                 , FolderRoute "date" Date
                 ]
             ]
        )
```

Do not get alarmed of the `deriving` clauses. Here we are deriving `IsRoute` *generically*, instead of hand-writing the instance. Deriving `IsRoute` gives us a *route encoder* (see [[route]] for details) that we can use to encode routes to URLs for use in the HTML of our site. 

What does it look like to derive `IsRoute` manually? Let's actually do it for the subroute `Date` type:


```haskell
import Data.Time
import Optics.Core (prism')

-- | Isomorphic to `Data.Time.Calendar.Day`
newtype Date = Date (Integer, Int, Int)
  deriving stock
    (Show, Eq, Ord, Generic)

instance IsRoute Date where
  type RouteModel Date = ()
  routePrism = mkRoutePrism $ \() ->
    prism'
      ( \(Date (y, m, d)) ->
          formatTime defaultTimeLocale "%Y-%m-%d.html" $
            fromGregorian y m d
      )
      ( fmap (Date . toGregorian)
          . parseTimeM False defaultTimeLocale "%Y-%m-%d.html"
      )
  routeUniverse _ = [] -- need model for this
```

1. We don't need any special data value to encode a `Day` route, thus `RouteModel` is a unit.
2. `mkRoutePrism` takes a function returning a `Prism` that knows how to encode and decode the `Day` route. The `Prism` is built using `formatTime` and `parseTimeM`.

The result is that we can use `routeUrl` to get the URL to our routes. In GHCi:

```haskell
ghci> import Ema.Route.Encoder (applyRoutePrism)
ghci> -- First get hold of the route Prism, which is passed to `siteOutput`
ghci> let routePrism = applyRoutePrism (routePrism @Route) ()
ghci> Ema.routeUrl routePrism Route_Index
"" -- The 'index.html' is dropped as it is redundant in HTML.
ghci> Ema.routeUrl routePrism $ Route_Day (Date 2022 04 23)
"date/2022-04-23.html" 
```

Route encoders are `Prism`s underneath. You can use optics operators to directly operate on them.

```haskell
-- Using `routePrism` from GHCi session above
ghci> import Optics.Core
ghci> review routePrism Route_Index
"index.html"
ghci> preview routePrism "2022-04-23.html"
Nothing
ghci> preview routePrism "date/2022-04-23.html"
Just (Route_Day (Date 2022 4 23))
ghci> 
```

See [[route]] for full details.