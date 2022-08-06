---
order: 7
---

# Composing Ema apps

As of 0.8 release, Ema apps can be composed together to create a larger app. See `Ex04_Multi.hs` in the Ema source tree for an example of this based on a top-level route ADT. The `Ex05_MultiRoute.hs` example is similar but uses the `MultiRoute` type instead of defining a new route ADT. The former has the main advantage of being able to use inner apps' models in defining the behaviour of other routes.
