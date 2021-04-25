# Working with routes

Ema gives you the freedom to use any Haskell type for representing your routes. You don't need complicated rewrite rules. Routes are best represented using what are known as *sum  types* (or ADT, short for *Abstract Data Type*). Here's an example of a route type:

```haskell
data Route 
  = Index
  | About
```

This type represents two routes pointing to -- the index page (`/`) and the about page (`/about`). Designing the route type is only half the job; you will also need to tell Ema how to convert it to / from the browser URL. This is achieve by writing an instance for the `Ema` typeclass:

```haskell
class Ema MyModel Route where 
  -- Convert our route to browser URL, represented as a list of slugs
  encodeRoute = \case
    Index -> []
    OnlyTime -> ["time"]
  -- Convert back the browser URL, represented as a list of slugs, to our route
  decodeRoute = \case
    [] -> Just Index
    ["time"] -> Just OnlyTime
    _ -> Nothing
```

(The `MyModel` type is explained in the [earlier section](howto/model.md)).

That is all there is to it. You can use whatever complex route types to model your website's routes, as long as those types are isomorphic to the slug list.

## The unit route

If you website has only one page `index.html` you can use `()` ("unit" type) as your route type. You'll still need write an instance for it.


{.last}
[Next]{.next}, with model and routes in place, [we will render HTML for the site](howto/render.md) using Ema.