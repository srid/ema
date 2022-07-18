# 01 Add Routes

We want to write a application to track moods, ie., a [mood tracking](https://en.wikipedia.org/wiki/Mood_tracking) app. 

The first step is to think about the various pages and define their corresponding route types. Our application will have an index page (displaying mood summary) as well as pages specific to the individual days.

```haskell
data Route
  = Route_Index
  | Route_Date Date
  deriving stock (Show, Eq, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute ''Route [t|'[]|]
```

We use TemplateHaskell to derive `IsRoute` *generically*, instead of hand-writing the instance. Deriving `IsRoute` gives us a [[prism]] that we can use to encode routes to URLs for use in the HTML of our site. 

We can of course also derive `IsRoute` manually. In fact, we must do it for the `Date` route type (as it is not an ADT):


```haskell
import Data.Time
import Optics.Core (prism')

-- | Isomorphic to `Data.Time.Calendar.Day`
newtype Date = Date (Integer, Int, Int)
  deriving stock
    (Show, Eq, Ord, Generic)

instance IsRoute Date where
  type RouteModel Date = ()
  routePrism () = toPrism_ $
    prism'
      ( \(Date (y, m, d)) ->
          formatTime defaultTimeLocale "%Y-%m-%d.html" $
            fromGregorian y m d
      )
      ( fmap (Date . toGregorian)
          . parseTimeM False defaultTimeLocale "%Y-%m-%d.html"
      )
  routeUniverse () = [] -- need model for this
```

1. We don't need any special [[model]] to encode a `Day` route, thus `RouteModel` is a unit.
2. `toPrism_` converts the optics-core `Prism'` into a coercible `Prism_` type that Ema internally uses. A route prism knows how to encode and decode the `Day` route. Our route `Prism'` is built using `formatTime` and `parseTimeM`.

The result is that we can use `routeUrl` to get the URL to our routes. In GHCi:

```haskell
ghci> -- First get hold of the route Prism, which is passed to `siteOutput`
ghci> let rp = fromPrism_ $ routePrism @Route ()
ghci> Ema.routeUrl rp Route_Index
"" -- The 'index.html' is dropped as it is redundant in HTML.
ghci> Ema.routeUrl rp $ Route_Day $ Date (2022, 04, 23)
"date/2022-04-23.html" 
```

You also can use [optics](https://hackage.haskell.org/package/optics-core) operators to directly operate on route prisms.

```haskell
-- Using `routePrism` from GHCi session above
ghci> import Optics.Core
ghci> review rp Route_Index
"index.html"
ghci> preview rp "2022-04-23.html"
Nothing
ghci> preview rp "date/2022-04-23.html"
Just (Route_Date (Date (2022,4,23)))
```

See [[route]] for full details.