---
order: 4
---

# Dynamic


`Dynamic`'s are essential to support [[hot-reload]].

A `Dynamic` is simply defined as:

```haskell
newtype Dynamic m a
  = Dynamic
      ( -- Initial value
        a
      , -- Set a new value
        (a -> m ()) -> m ()
      )
```

It is a pair of values: the initial value, and a function that knows how to update that value over time using the user-provided update function (`a -> m ()`). Dynamic's are an `Applicative`, so they compose using `liftA*` family of functions. 

The [[site]]'s `siteInput` method returns a `Dynamic` of [[model]], which represents all the data required to render a site. If you do want [[hot-reload]], you may return a `pure` value.

The use of a time-varying `Dynamic` is what enables [[hot-reload]]. See [here](https://github.com/fpindia/fpindia-site/pull/24/files) for an example of making a model time-varying. Checkout [[unionmount]] to produce a `Dynamic` of a model that updates based on filesystem tree.