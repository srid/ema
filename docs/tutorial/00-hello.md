---
order: 0
---

# Hello World

To write a minimal Ema app, we need two things at minimum: a [[route]] type corresponding to the generated HTML file(s) (`Route` below) as well as an [[site|EmaSite]] instance on that route type defining the site render pipeline. The simplest Ema app is presented below:

```haskell
import Ema

newtype Route = Route ()
  deriving newtype
    (Show, Eq, Ord, Generic, IsRoute)

instance EmaSite Route where
  siteInput _ _ =
    pure $ pure ()
  siteOutput _ _ _ =
    pure $ Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = void $ Ema.runSite @Route ()
```

Let's walkthrough this code:

1. `Route` is a singleton type, because our site has exactly one page -- `index.html`. 
    - The unit type, `()`, already has an `IsRoute` instance, so we derive it via newtype.
    - In [[01-routes]], you will see how to write more elaborate route types and derive `IsRoute` for them.
1. `EmaSite`'s `siteOutput` method is used to render this route. 
    - `siteInput` is responsible for returning the data value (model) used in rendering the routes. In [[02-model]] we will use a custom model, and in [[03-dynamic]] we will make it time-varying.
1. `Ema.runSite` takes a route type (via `TypeApplications`), and runs the Ema site. 
   1. Running the resultant executable without arguments runs the [[live-server]], whereas running it with the `gen` subcommand will generate the static site (see [[cli]]).

{.last}
[Next]{.next}, [[01-routes|we will explain]] how to write a simple **mood tracker** in Ema.