---
order: 1
---

# Route type

[Algebraic datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type) (ADT) are well suited to model website routes. For example, the following type can be used to model routes in a weblog site with "About" and "Contact" pages:

```haskell
data Route
  = Route_Index
  | Route_About
  | Route_Contact
  | Route_Blog BlogRoute

data BlogRoute
  = BlogRoute_Index
  | BlogRoute_Post Slug

newtype Slug = Slug { unSlug :: String }
```

As you can see, routes can be *nested*. `BlogRoute` is a *subroute* of the `Route` type. When encoding `Route`, it can delegate to the encoder for `BlogRoute` (recursively). 

## `IsRoute`

To make the above type an Ema route, you will define an `IsRoute` instance for it. The principle functionality of this typeclass is to provide a [[prism]] (`Prism' FilePath r`).

1. `RouteModel` is used to specify the value ([[model]]) that is used for encoding routes (as defined by `routePrism`). In a real site, this would be a type that contains your blog posts.
2. `routePrism` gives us an optics prism. See [[prism]] for details.
3. `routeUniverse` simply returns a list of routes to statically generate.

### Generic deriving

You do not always have to write `IsRoute` by hand. See [[generic]] for details.
