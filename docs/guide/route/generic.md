---
order: 2
---

# Generic deriving

[[class]] can be derived generically using `DerivingVia`. 

Let's see how it looks using the blog website routes (shown below). 

![[example]]

Typically, the terminal sub-routes will require a hand-written instance. For eg., the `Slug` type will need a `IsRoute` instance as follows:

```haskell
instance IsRoute Slug where
  type RouteModel Slug = ()
  routePrism () = toPrism_ $ prism' (<> ".html") parsePostSlug
  routeUniverse () = []
```

The higher level routes (`BlogRoute` and `Route`) can be derived automatically using generics via `DerivingVia`, for instance:

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

The TH way has better compiler error messages due to the use of standalone deriving.

## How generic deriving works

Ema uses [`generics-sop`](https://hackage.haskell.org/package/generics-sop). The implementation is delegated to `Ema.Route.Lib.Multi.MultiRoute`, which is a generic route type based on `NS` and `NP` from `sop-core`. Thus, much of generics machinary involves converting user's route type to `MultiRoute`; to do this, we must derive instances for `HasSubRoutes` and `HasSubModels`.

### `HasSubRoutes`: `FileRoute` and `FolderRoute`

`HasSubRoutes` gives us an isomorphism between the route type's sum constructors and `FileRoute` or `FolderRoute` types. For example, the `BlogRoute` type above is converted to:

```haskell
type BlogRoute' = 
  MultiRoute 
    '[ FileRoute "index.html"
     , FolderRoute "blog" Slug
     ]
```

Notice how the "shape" of the two types match. Constructors with zero arguments (`BlogRoute_Index`) are isomorphic to `FileRoute`, whereas constructors with one argument (`BlogRoute_Post Slug`) are isomorphic to `FolderRoute a` (where `a` is that argument type). Route constructors cannot not have more than one argument.

#### `WithSubRoutes`

The `WithSubRoutes` option to `GenericRoute` can be powerful if you want to use something other than `FileRoute`/`FolderRoute` in the generic deriving (but without needing to hand-write encoders and decoders). 

- `FileRoute` can be used to provide a specific filepath for a route constructor without arguments
- `FolderRoute` can do the same for a route constructor with an unary argumenmt. 

In GHC 9.2+, `WithSubRoutes` is generically determined in this manner. A constructor like `Route_Blog BlogRoute` automatically expands to `FolderRoute "blog" Slug`. 

You can use any arbitrary type as long as they are coercible. In effect, `WithSubRoutes` enables "deriving [HasSubRoutes] via" the specified isomorphic route constructor representations.

### `HasSubModels`

`HasSubModels` does for `RouteModel` what `HasSubRoutes` does for route constructors. In many simple cases your sub-routes share the same model type as the larger route, but in some cases you want to have a different model type for each sub-route. 

To generically achieve this, we want to be able to extract the sub-model from the larger model. `HasSubModel` provides this functionality via [`HasAny`](https://hackage.haskell.org/package/generic-optics-2.2.1.0/docs/Data-Generics-Product-Any.html). 


#### `WithSubModels`

Like, `WithSubRoutes` you can explicitly specify the sub-model to extract (via `HasAny` for instance) if there are ambiguities.

See Ex03_Store.hs in Ema source tree for an example.

## Custom generic options

To further customize the behaviour of `IsRoute` generic deriving, you can define your own options for `GenericRoute`. To do this, you simply need to write an instance of `GenericRouteOpt` for your option type.

