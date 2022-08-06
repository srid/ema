# Route Prism

Route types are associated with an [optics `Prism'`](https://hackage.haskell.org/package/optics-core-0.4.1/docs/Optics-Prism.html#t:Prism-39-) (`Prism' FilePath r`) that in turn can be used to encode and decode route values to and from URLs or filepaths. 

Here is a naive implementation for `BlogRoute` from the example in [[route]]:

```haskell
instance IsRoute BlogRoute where
  type RouteModel BlogRoute = ()
  routePrism () = toPrism_ $
    prism'
      ( \case 
          BlogRoute_Index -> "index.html"
          BlogRoute_Post slug -> "posts" </> slug <> ".html"
      )
      ( \case
          "index.html" -> Just BlogRoute_Index
          (parsePostSlug -> Just slug) -> Just $ BlogRoute_Post slug
          _ -> Nothing
      )
  routeUniverse () = []
```

`routePrism` can be generically determined for routes with "standard shapes"; see [[generic]].

### optics-core

Ema uses the `optics-core` package (instead of `lens`), which provides [the `Prism'` type](https://hackage.haskell.org/package/optics-core-0.4.1/docs/Optics-Prism.html#t:Prism-39-). You can think of `routePrism` as returning a `Prism' FilePath r` in effect -- allowing us to convert between a route value and a filepath. In reality, `routePrism` must return a `Prism_` type that Ema provides. 

`Prism_` is isomorphic to `Prism'` -- with conversion functions `toPrism_` and `fromPrism_`. The typeclass is obliged to use `Prism_` instead of `Prism'` due to a Haskell limitation in regards to DerivingVia and coercions (see [details here](https://stackoverflow.com/q/71489589/55246)).