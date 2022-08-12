---
order: 3
---

# Model type

A "model", in an Ema app, is a type that is required to render the site. The model is also used, optionally, to encode the [[route|route types]].

Taking the blog site example from [[example]], our model would look something like the following:

```haskell
data Model = Model 
  { modelSiteTitle :: Text 
  , modelBlogPosts :: Map Slug (Date, Pandoc)
  }
```

Models impact two places:

1. `RouteModel` of [[class]] associates a route type with a model type. This enables [[prism]] (and `routeUniverse`) to accept that model value as an argument.
2. In [[site]] typeclass, `siteInput` can now return this model value (and it can be time-varying if using a [[dynamic]]), as well as `siteOutput` can take the model value so as to render the site based on it.

## Useful libraries

- [ixset-typed](https://github.com/well-typed/ixset-typed) - for database-like querying into in-memory model values
- [[unionmount]] - mounting local files into Haskell in-memory model with change updates.