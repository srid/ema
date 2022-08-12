# Route Prism

Deriving [[class]] for our route types gives us a `routePrism` function that returns (effectively[^prism]) a `Prism' FilePath r` (from [optics-core `Prism'`](https://hackage.haskell.org/package/optics-core-0.4.1/docs/Optics-Prism.html#t:Prism-39-)) that in turn can be used to encode and decode route values to and from URLs or filepaths. 

Let's consider the example route,

![[example]]

Here is a naive implementation of [[class]] for the `BlogRoute` above:

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

In GHCi you can play with this prism as,

```haskell
ghci> let rp = routePrism @BlogRoute ()
ghci> preview rp "posts/foo.html"
Just (BlogRoute_Post (Slug "foo"))
ghci> review rp $ BlogRoute_Post (Slug "foo")
"posts/foo.html"
```

Ema provides a `routeUrl` function that converts this filepath to an URL.

## Generic prism

`routePrism` can also be generically determined for routes with "standard shapes" (both `Route` and `BlogRoute` above); see [[generic]].


[^prism]: 
    In reality, `routePrism` must return a `Prism_` type that Ema provides. 

    `Prism_` is isomorphic to `Prism'` -- with conversion functions `toPrism_` and `fromPrism_`. The typeclass is obliged to use `Prism_` instead of `Prism'` due to a Haskell limitation in regards to DerivingVia and coercions (see [details here](https://stackoverflow.com/q/71489589/55246)).