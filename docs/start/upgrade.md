---
order: 100
---

# Upgrading

Until 1.0 is released, newer releases of Ema may see breaking or significant changes. This page exists to help you with upgrading your apps to the subsequent versions.

## 0.6 -> 0.8

`0.8` is a *substantial* change, with Ema getting nearly rewritten entirely. See [PR \#81](https://github.com/srid/ema/pull/81) called 'Multisite rewrite'. 

First, read [[tutorial]] to get a taste of the new API. Then, in your old Ema site:

- Extract `encodeRoute` and `decodeRoute` (of `Ema` instance) into an [[class]] instance.
- Change `Ema` to `EmaSite`, and define `siteInput` (what used to be in argument to `runEma`) and `siteOutput` (the render argument to `runEma`). You will want to use [[dynamic]] instead of `LVar`.
- `Ema.routeUrl`: change to accept the [[prism]] that is now passed to `siteOutput`


### Apps that already use Ema 0.8

You may want to read the source code of these apps for more context.

- https://github.com/fpindia/fpindia-site (prefer this one)
- https://github.com/srid/emanote
- https://github.com/srid/timedot-invoice
- https://github.com/JonathanReeve/jonreeve.com-ema