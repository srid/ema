---
order: 6
---

# Asset

An "asset" is simply a `ByteString` (written to disk) or `FilePath` (copied) that `siteOutput` of [[site]] typically produces. It is defined as:

```haskell
-- | The type of assets that can be bundled in a static site.
data Asset a
  = -- | A file that is copied as-is from the source directory.
    --
    -- Relative paths are assumed relative to the source directory. Absolute
    -- paths allow copying static files outside of source directory.
    AssetStatic FilePath
  | -- | A file whose contents are generated at runtime by user code.
    AssetGenerated Format a
  deriving stock (Eq, Show, Ord, Functor, Generic)

-- | The format of a generated asset.
data Format
  = -- | Html assets are served by the live server with hot-reload
    Html
  | -- | Other assets are served by the live server as static files.
    Other
  deriving stock (Eq, Show, Ord, Generic)
```

## HTML asset 

[[live-server]] specifically looks at the `Asset Generated . Html` asset and [[hot-reload]]s the browser on its change. 

## Static file asset 

The `AssetStatic fp` asset is copied as-is from the source directory to the destination directory. In [[live-server]] the file is served directly from the given path. See `Ema.Route.Lib.Extra.StaticRoute` for example.

## Other assets

Any generated route that is not HTML (eg: RSS) must use the `AssetGenerated . Other` type.