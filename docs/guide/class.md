---
order: 3
---
# Defining Ema instance

Once you have [model](guide/model.md) and [route](guide/routes.md) types in place, we must tell the Haskell compiler that they are suitable for generating static sites. We do this by creating an instance of the `Ema` typeclass.

Using some `MyModel` and the route `Route` shown in the [previous](guide/routes.md) section, we can create an instance as follows:

```haskell
class Ema MyModel Route where 
  -- Where to generate this route?
  encodeRoute = \case
    Index -> "index.html"
    About -> "about.html"

  -- Which route does this filepath correspond to?
  decodeRoute = \case
    "index.html" -> Just Index
    "about.html" -> Just About
    _ -> Nothing

  -- The third method is optional, and used by the `gen` command (not live-server)
  -- By default, Enum & Bounded will be used to determine this list.
  allRoutes model =
    [Index, About]
```

The `Ema` typeclass has three methods, one of them being optional:

1. Define `encodeRoute` that converts our route type to a filepath 
2. Define `decodeRoute` that does the *reverse* converstion (the conversion must be isomorphic)
3. _Optionally_, define `allRoutes` indicating the routes to statically generate

## `runEma`

The `Ema` constraint is used by the `runEma` function that acts as the main entry point to your static site generator. It takes two arguments:

1. `render` function that renders your route (usually to HTML); we'll go over this in [the next](guide/render.md) section.
2. an IO action that takes [`LVar model`](guide/model.md) as an argument. 
 
This IO action is expected to be a long-running one, wherein you have full control over setting the value of the model over time.

{.last}
[Next]{.next}, with our model and routes in place constrained by `Ema` type class, [we will define the HTML for our site](guide/render.md) using Ema.
