---
order: 7
---

# Composing Ema apps

Smaller Ema apps can be composed together to create a larger Ema app. 

The composition happens via sub-route composition. i.e., the [[route]] of your larger Ema app can contain the sub-route type in its sum constructors. If the sub-routes have [[class]] and [[site]] instances, then the corresponding instances for the larger route can be defined *in terms of* them.

For example, consider the note-publishing app [Emanote](https://emanote.srid.ca/), which is written in Ema. If you are creating your own personal website in Ema, and wish to include the Emanote generated site in a sub-path, say `/notes/`, you can do that by composing the Emanote app within your own Ema app.

## Examples

See `Ex04_Multi.hs` in the Ema source tree for an example of this based on a top-level route ADT. The `Ex05_MultiRoute.hs` example is similar but uses a heterogenous list (`NS` from sop-core), that is used in [[generic]], instead of defining a new route ADT. The former has the main advantage of being able to use inner apps' models in defining the behaviour of other routes.

## Builtin sub-apps

Ema provides a few "sub apps" that are useful in developing static sites.

### `StaticRoute`

`Ema.Route.Lib.Extra.StaticRoute` provides a `StaticRoute` site that can monitor a directory and allow referring to the files in it. The [[model]] for this sub-route keeps track of last-added time, which gets appended to the URL when running in [[live-server]], which in turn has the effect of "refreshing" the target content (eg: image) when the underlying files change.

An example of its use can be seen here: https://github.com/EmaApps/ema-template

### `PandocRoute`

`Ema.Route.Lib.Extra.PandocRoute` provides a `PandocRoute` site that can monitor a directory of LML files (Markdown, org-mode, etc.) and return (in `siteOutput` of [[site]]) the parsed Pandoc AST along with a function that renders it to HTML.

An example of its use can be seen here: https://github.com/fpindia/fpindia-site/pull/21