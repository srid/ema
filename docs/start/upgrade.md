---
order: 100
---

# Upgrading to newer Ema

Until 1.0 is released, newer releases of Ema may see breaking or significant changes. This page exists to help you with upgrading your apps to the subsequent versions.

## 0.6 -> 0.8

`0.8` is a *substantial* change, with Ema getting nearly rewritten entirely. See [PR \#81](https://github.com/srid/ema/pull/81) called 'Multisite rewrite'. 

TODO: How to upgrade to 0.8?

- `RouteEncoder` (first-class route encoders)
    - Pass it around, for use in `Ema.routeUrl`, etc.
- `EmaSite` defines both "input" (model dynamic) and "output" (html rendering)


### Apps that already use Ema 0.8

You may want to read the source code of these apps for more context.

- https://github.com/srid/emanote
- https://github.com/srid/timedot-invoice
- https://github.com/fpindia/fpindia-site