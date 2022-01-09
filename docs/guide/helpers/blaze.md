---
order: 1
---
# Using Blaze HTML & Tailwind

## Tailwind 2.0

The `Ema.Helper.Blaze` module provides a `twindLayout` function that uses [twind](https://twind.dev/) shim that is used in the statically generated site, and otherwise uses Tailwind CSS from CDN in the dev server mode. This helper is for those that **use [Tailwind CSS](https://tailwindcss.com/) in conjunction with [blaze-html](https://hackage.haskell.org/package/blaze-html) DSL**.

To use the layout helper in your [render](guide/render.md) function:

```haskell
render :: Some Ema.CLI.Action -> MyModel -> MyRoute -> Asset LByteString
render emaAction model route = do
  AssetGenerated Html $ 
    Blaze.twindLayout emaAction (H.title "My site" >> H.base ! A.href "/") $ do 
      H.p "Hello world"
```


## Tailwind 3.0 

> **Note** that because the above [twind JS shim](https://twind.dev/handbook/the-shim.html) is used to support Tailwind styles your site will not render properly on web browsers with JavaScript disabled if you use this helper; it might also have trouble interoperating with other JS initializers on the site. See [this issue](https://github.com/srid/ema/issues/20) for upcoming alternatives.

For new Ema sites, it is recommended to use Tailwind 3.0 both in live server and static site generation. See [ema-template](https://github.com/srid/ema-template) for an approach to this.[^pr]

[^pr]: [Here](https://github.com/srid/ema-template/pull/16) is the specific PR making this change.
