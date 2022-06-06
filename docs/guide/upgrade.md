
# Upgrading to newer Ema

## 0.6 -> 0.8

`0.8` is a *substantial* change, with Ema almost rewritten entirely. See [PR #81](https://github.com/srid/ema/pull/81) called 'Multisite rewrite'. If you do not wish to upgrade, you should stay with version `0.6`.

TODO: How to upgrade to 0.8?

- `RouteEncoder` (first-class route encoders)
    - Pass it around, for use in `Ema.routeUrl`, etc.
- `EmaSite` defines both "input" (model dynamic) and "output" (html rendering)