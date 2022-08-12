---
order: -1
---

# `IsRoute`

The `IsRoute` typeclass is used to mainly define a [[prism]]-based encoding for your route types. In other words, deriving `IsRoute` for the example route,

![[example]]

enables us to convert between `Route_Blog BlogRoute_Index` and `/blog/index.html` and vice versa (as shown in [[01-routes|the tutorial]]).

## Explanation

`IsRoute` is defined as:

```haskell
class IsRoute r where
  -- (1)
  type RouteModel r :: Type
  -- (2)
  routePrism :: RouteModel r -> Prism_ FilePath r
  -- (3)
  routeUniverse :: RouteModel r -> [r]
```

1. `RouteModel` is (optionally) used to specify the [[model|value]] that is used for encoding routes (as defined by `routePrism`). 
   1. For the blog route example above, this maybe be a type that contains your blog posts. It can also be `()` if you do not care about it.
2. `routePrism` gives us an optics prism (see [[prism]]) that can be used to encode and decode between the route type and the `.html` filepath. 
3. `routeUniverse` simply returns a list of routes to statically generate.

## Generic deriving

`IsRoute` may be derived genericallly; see [[generic]].