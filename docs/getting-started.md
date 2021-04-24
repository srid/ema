# Getting Started

Ema is heavily a work in progress. Version 1.0 release is expected soon. If you wish to adopt it early for your projects, start from the unofficial template repo [srid/orgself](https://github.com/srid/orgself). 

The simplest Ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Ema"
  runEmaPure $
    encodeUtf8 $ "<b>Hello</b>, from " <> name
```

{.last}
[Next]{.next}, we will [develop a basic site](getting-started/basic-site.md) using Ema.