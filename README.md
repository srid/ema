# ema

<img width="10%" src="./docs/ema.svg">

[![Hackage](https://img.shields.io/hackage/v/ema.svg?logo=haskell)](https://hackage.haskell.org/package/ema)

Ema is a next-gen **Haskell** library for building [jamstack-style](https://jamstack.org/) static sites, with fast hot reload. See [ema.srid.ca](https://ema.srid.ca/) for further information.

The simplest Ema app looks like this:

```haskell
main :: IO ()
main = do
  let name :: Text = "Ema"
  runEmaPure $ \_ ->
    encodeUtf8 $ "<b>Hello</b>, from " <> name
```

## Hacking

Run `bin/run` (or <kbd>Ctrl+Shift+B</kbd> in VSCode). This runs the documentation example; modify `./.ghcid` to run a different example, such as the clock example - which updates every second, demonstrating hot reload.
