---
order: 2
---

# GenericRoute

`IsRoute` can be derive generically using `DerivingVia`. 

Let's see how it looks using the blog website routes from [[route]]. Typically, the terminal sub-routes will require a hand-written instance. For eg., the `Slug` type will need a `IsRoute` instance as follows:

```haskell
instance IsRoute Slug where
  type RouteModel Slug = ()
  routePrism () = toPrism_ $ prism' (<> ".html") parsePostSlug
  routeUniverse () = []
```

And the higher level routes can be derived automatically using generics via `DerivingVia`, for instance:

```haskell
data BlogRoute
  = BlogRoute_Index
  | BlogRoute_Post Slug
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving
    (HasSubRoutes, HasSubModels, IsRoute)
    via ( GenericRoute
            BlogRoute
            '[ WithModel ()
             , -- Not needed in GHC 9.2
               WithSubRoutes
                '[ FileRoute "index.html"
                 , FolderRoute "blog" Slug
                 ]
             ]
        )
```

Note that `WithSubRoutes` is automatically computed in GHC 9.2 or above. `WithModel` defaults to `()`. So, in GHC 9.2, you can also write `deriving ... via (GenericRoute BlogRoute '[])`

## TemplateHaskell

Ema also provides a TH wrapper for the above `GenericRoute` deriving. In the blog example, we can derive `IsRoute` for the top-level `Route` type as follows:

```haskell
data Route
  = Route_Index
  | Route_About
  | Route_Contact
  | Route_Blog BlogRoute
  deriving stock (Show, Eq, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute ''Route [t|'[WithModel ()]|]
```

## `WithSubRoutes`: `FileRoute` and `FolderRoute`

The `WithSubRoutes` option to `GenericRoute` can be powerful if you want to specify a custom encoding in the generic deriving (but without needing to hand-write encoders and decoders). `FileRoute` can be used to provide a specific filepath for a route constructor without arguments, and `FolderRoute` can do the same for a route constructor with an unary argumenmt. In GHC 9.2+, `WithSubRoutes` is generically determined in this manner. A constructor like `Route_Blog BlogRoute` automatically expands to `FolderRoute "blog" Slug`. 

You can use any arbrirary type as long as their generic representations are isomorphic (per the `GIsomorphic` class). In effect, `WithSubRoutes` enables "deriving [HasSubRoutes] via" the specified isomorphic route constructor representations.

## `HasSubModel`

The `HasSubModel` option to `GenericRoute` is relevant when your subroutes specify a model *different* to the top-level route (see Ex03_Store.hs in Ema source tree for an example). It tells the generic deriving system how to "break" the top-level model into submodels corresponding to the subroutes. Ema's generic deriving mechanism relies on [`HasAny`](https://hackage.haskell.org/package/generic-optics-2.2.1.0/docs/Data-Generics-Product-Any.html) from `generic-optics` for large part to determine this automatically, and the `WithSubModels` option can be used to explicitly specify the lenses if there are ambiguties. You can of course also derive `HasSubModels` manually.

## Custom generic options

To further customize the behaviour of `IsRoute` generic deriving, you can define your own options for `GenericRoute`. To do this, you simply need to write an instance of `GenericRouteOpt` for your option type.

