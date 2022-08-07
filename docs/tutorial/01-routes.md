---
order: 1
---

# Add Routes

We want to write an application to track moods, ie., a [mood tracking](https://en.wikipedia.org/wiki/Mood_tracking) app. 

The first step is to think about the various pages and define their corresponding route types. Our application will have an index page (displaying mood summary) as well as pages specific to the individual days.

```haskell
data Route
  = Route_Index
  | Route_Date Date
  deriving stock (Show, Eq, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute ''Route [t|'[]|]
```

We use TemplateHaskell to derive `IsRoute` *generically*, instead of hand-writing the instance. We derive `IsRoute` to enrich our route type with three capabilities:

1. `RouteModel`: associate a value type ([[model]]) that is used for encoding routes
2. `routePrism`: produce [[prism]] (a `Prism'`) that we can use to encode routes to URLs and vice versa. 
3. `routeUniverse`: generate a list of routes to statically generate

We can of course also derive `IsRoute` manually. In fact, we must do it for the `Date` sub-route type (as it is not an ADT shaped for generic deriving):


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

1. We don't need any special [[model]] to encode a `Day` route, thus `RouteModel` is a unit. But we'll modify this in next step (to implement `routeUniverse`).
2. `toPrism_` converts the optics-core `Prism'` into a coercible `Prism_` type that Ema internally uses. A route prism knows how to encode and decode the `Day` route. Our route `Prism'` is built using `formatTime` and `parseTimeM`.
3. We will implement `routeUniverse` in the next step of the tutorial

The result is that we can use the function `routeUrl` to get the URL to our routes. Let's see this in action in GHCi:

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
-- NOTE: Using `rp` from GHCi session above
ghci> import Optics.Core
ghci> review rp Route_Index
"index.html"
ghci> preview rp "2022-04-23.html"
Nothing
ghci> preview rp "date/2022-04-23.html"
Just (Route_Date (Date (2022,4,23)))
```

See [[route]] for details.

{.last}
[Next]{.next}, [[02-model|we will explain]] how to define our `moods.csv` model and render it .