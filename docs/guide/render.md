# Rendering HTML

Once you have [model](guide/model.md) and [routes](guide/routes.md) in place, the last piece of the puzzle is to write a function that takes both as arguments and returns the HTML string (lazy bytestring, to be exact). This function can be as simple as the following:

```haskell
render :: MyModel -> Route -> ByteString
render model route =
  "<b>Hello</b>, world!"
```

Of course we want it to be real, by using our model value, as well as generate the HTML based on the route. We will also use the [blaze-html](https://hackage.haskell.org/package/blaze-html) library to make writing HTML in Haskell palatable. A more realistic starting point (if not the finishing product) would be:

```haskell
render :: MyModel -> Route -> ByteString 
render model route = Blaze.renderHtml $ 
  H.html $ do 
    H.head $ do 
      H.title "My site"
    H.body $ do 
      H.h1 "My site"
      case route of 
        Index -> 
          H.h1 "Welcome to my website!"
          H.p $ do 
            "Checkout the"
            H.a ! A.href (Ema.routeUrl About) $ "About"
            " page."
        About ->
          H.div $ H.p "This website is managed by yours truly"
      H.footer $ do 
        A.a ! A.href "https://github.com/user/repo" $ "Source on GitHub"
```

Note that Ema provides a `routeUrl` helper function that serializes your route to the final URL (here, `/about`) for linking to.

Spend a few moments to recognize how this is *much simpler* to write than dealing with HTML template files spread across the disk. Besides, Haskell's type-safety now applies to your HTML as well. On top of it, Ema's hot-reload will instantly update the dev server's browser view whenever you change your HTML (or any of the Haskell source code).


{.last}
[Next]{.next}, you might want to peruse [the helper topics](guide.md) in the parent section.