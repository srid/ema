# 00 Hello World

Writing a minimal Ema app involves at minimum a [[route]] type corresponding to the generated HTML file (`Route` below) as well as an [[site|EmaSite]] instance on that route type defining the site render pipeline. The simplest Ema app is presented below:

```haskell
newtype Route = Route ()
  deriving newtype
    (Show, Eq, Ord, Generic, IsRoute)

instance EmaSite Route where
  siteInput _ _ =
    pure $ pure ()
  siteOutput _ _ _ =
    Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = void $ Ema.runSite @Route ()
```

Let's walkthrough this code:

1. `Route` is a singleton type, as our site has exactly one page -- `index.html`. 
    - The unit type, `()`, already has a `IsRoute` instance, so we newtype it.
    - In [[01-routes]], you will see how to write more elaborate route types.
1. `EmaSite`'s `siteOutput` method is used to render this route. 
    - Our trivial site has `()` as its [[model]], therefore `siteInput` returns a constant [[dynamic]] of `()`. In [[02-model]] we will use a custom model, and in [[03-dynamic]] we will make it time-varying.
1. `Ema.runSite` takes a route type, and runs the Ema site. 
   1. If you build this program, running it without arguments runs the [[live-server]], where as running it with the `gen` command will generate the static site.

The rest of the tutorial series will explain how to write a simple **mood tracker** in Ema.