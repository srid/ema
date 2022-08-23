
# Hello World

Writing an Ema apps involves two things at a minimum: 
- a [[route]] type corresponding to the generated HTML file(s), as well as 
- an [[site|EmaSite]] instance on that route type defining the site render pipeline. 

The simplest Ema app is presented below:

```haskell
import Ema

-- The Route type of our site
newtype Route = Route ()
  deriving newtype
    (Show, Eq, Ord, Generic, IsRoute)

-- Site pipeline (input & output)
instance EmaSite Route where
  siteInput _ _ =
    -- There is no input in a hello-world site
    pure $ pure ()
  siteOutput _ _ _ =
    -- The output of index.html is simply a hello-world message in HTML
    pure $ Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = 
  -- Hook everything up in main using runSite.
  void $ Ema.runSite @Route ()
```

Let's walk through this code:

1. The `Route` type represents the pages on our site. As there is only one page (`index.html`) in our hello-world site, we simply use `()`. 
    - The unit type, `()`, already has an `IsRoute` instance, so we derive it via `newtype`.
    - In [[01-routes]], you will see how to write more elaborate route types and derive `IsRoute` for them. `IsRoute` is what tells Ema that a Haskell type is a route type (with URL encoders and decoders).
1. The `EmaSite` typeclass defines the "site pipeline" -- the input [[model]] and the output [[asset]]:
   1. `siteOutput` renders this route. 
   1. `siteInput` returns the [[model|model]] used in rendering the routes. In [[02-model]] we will use a custom model, and in [[03-dynamic]] we will make it time-varying.
   1. `Ema.runSite` takes a route type (via `TypeApplications`), and runs the Ema site. 
1. Running the resultant executable without arguments runs the [[live-server]], whereas running it with the `gen` subcommand will generate the static site (see [[cli]]).

{.last}
[Next]{.next}, [[01-routes|we will explain]] how to write a simple **mood tracker** in Ema.