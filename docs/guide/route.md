---
order: 1
---

# Route type

[Algebraic datatypes](https://en.wikipedia.org/wiki/Algebraic_data_type) (ADT) are well suited to define website routes in Haskell. For example, the following type represents routes in a particular weblog site:

![[example]]

As you can see, routes can be *nested*. `BlogRoute` is a *subroute* of the `Route` type. When encoding `Route`, it can delegate to the encoder for `BlogRoute` (inductively). 

## Routes are central to Ema apps

An Ema app is defined by its route type. All routes must be an instance of the [[class]] class, which provides a route encoder (see [[prism]]) that is used to convert to and from the corresponding `.html` filepaths. This instance can be hand-written or [[generic|derived generically]].
