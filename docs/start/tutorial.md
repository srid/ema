# Tutorial

Make sure that you have have followed [[start|the previous section]] in order to have the [template repo](https://github.com/srid/ema-template) checked out and running locally. Here, **our goal** is to replace the source code of the template repo and write a basic site from scratch.

1. Follow the template repo's [README](https://github.com/srid/ema-template#getting-started) and have it open in Visual Studio Code while running the dev server. Your website should be viewable at <http://localhost:9001/>
1. Open `src/Main.hs`
1. Delete everything in it, and replace it with the following

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Ema
import Generics.SOP qualified as SOP

data Route = Route_Index
  deriving stock (Show, Eq, Ord)
deriveGeneric ''Route
deriving anyclass instance IsRoute Route

instance EmaSite Route where
  siteOutput _enc _m Route_Index =
    Ema.AssetGenerated Ema.Html "<b>Hello</b>, Ema"

main :: IO ()
main = void $ Ema.runSite @Route ()
```

The above is the *minimum* amount of code necessary to run an Ema site. Notice that as you replace and save this file, your browser (which is at <http://locahost:9001>) will [[hot-reload|hot reload]] to display "Hello, Ema". Congratulations, you just created your first website! 

## Expanding on Hello World

Okay, but that's just *one* page. But we want to add a second page. And we might as well add more content than "Hello, Ema". Let's do that next. The first step is define the [[routes|route]] type that corresponds to our site's pages. Add the following:

```haskell
data Route
  = Route_Index -- Corresponds to /
  | Route_About -- Corresponds to /about
  deriving stock
    (Show, Eq, Ord, Generic)
  deriving anyclass
    (SOP.Generic, SOP.HasDatatypeInfo, IsRoute)
```

Note that we derive `IsRoute` generically (via SOP instances) which in turn gives us free route encoding and decoding. Here, `Route_Foo` encodes to `/foo.html`.

Next, let's define a [[model|model]]. A model will hold the state of our website used to render its HTML. Let's put the `speaker` variable in it, as that's all we are using:

```haskell
data Model = Model { speaker :: Text }
```

Now that we have defined both our `Route` and `Model` types, it is time to connect everything up to define the site pipeline. This is done by creating an instance of the `EmaSite` [[class|typeclass]].

```haskell
instance EmaSite Route where
  siteInput _ _ () = do
    let model0 = Model "me"
    pure $ Dynamic $ (model0,) $ \setModel -> do
      -- A long-running IO action that updates the model.
      forever $ do
        liftIO $ threadDelay 1000000
        t <- liftIO getCurrentTime
        setModel $ Model $ show t
  siteOutput enc m r =
    Ema.AssetGenerated Ema.Html $ render enc m r
```

The `EmaSite` typeclass provides two methods: `siteInput` and `siteOutput`, each definitning the input data and output asset respectively. `siteInput` provides a `Dynamic` (ie., time-varying) of the `Model` value that in turn is passed to the `siteOutput` function which also takes the `Route` value for which we are to generate the content to write to. Since our `Route` type represent a HTML page, we will write HTML bytestring that will be returned by the `render` function.

```haskell
import Text.Blaze.Html.Renderer.Utf8 qualified as RU
import Text.Blaze.Html5 qualified as H

render :: RouteEncoder Model Route -> Model -> Route -> LByteString
render enc model r = do
  RU.renderHtml $ do
    H.docType
    H.html $ do
      H.body $ do
        case r of 
          Route_Index -> do
            H.p $ H.b $ H.text $ speaker model 
            H.p $ H.a ! A.href (Ema.routeUrl enc model Route_About) $ H.text "About"
          Route_About -> "About page"
```

If everything compiles, you should see the site update in the web browser. A couple of quick points about the `render` function:

1. It should return the raw HTML as a `ByteString`. Here, we use [blaze-html](https://hackage.haskell.org/package/blaze-html) as HTML DSL. You can also use your own HTML templates of course.
1. It uses `Ema.routeUrl` function to create a URL out of our `Route` type. This function uses the `RouteEncoder` returned by the [[what|`IsRoute` typeclass]] instance.

On final note, you will note that nothing is actually *generated* so far. This is because Ema has been running in the dev server mode, which is quite useful during development. To actually generate the files, you can use the `gen` command when running the [[cli|CLI]]:

```sh
mkdir ~/output
nix run . -- gen $HOME/output
```

## Exercises

1. Discover how to manage static files like images and PDFs (hint: see `AssetStatic` in `Ema.Asset`)
2. What happens if you `throw` an exception or use `error` in the `render` function?

{.last}
[Next]{.next}, checkout the [[guide]] series for information on specific topics.
