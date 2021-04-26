# Defining your model

TODO

- What's a "model"?
  - Represents all the 'state' in your app.
- Any Haskell type
- Stored in `LVar` to enable hot-reload
- `LVar.set` and `LVar.modify`
  - When the program starts up, set the initial value
  - Over time, use LVar.modify (or LVar.set) to update it based on changes (eg: filesystem)
- `runEma`

{.last}
[Next]{.next}, we will [talk about routes](guide/routes.md).