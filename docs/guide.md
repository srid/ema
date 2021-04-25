# Guide

After having familiarized yourself with Ema by following the [earlier section](start.md), you are now ready to dive deep into learning how to achieve specific things.

* [Defining your model](guide/model.md) -- [Define your site model such that it supports hot reload]{.item-intro}
* [Working with routes](guide/routes.md) -- [Unless you site has a single page (`index.html`), you will need to manage a set of routes]{.item-intro}
* [Rendering HTML](guide/render.md) -- [You could use plain strings to build HTML, or use templates, or use one of the delightful Haskell DSLs]{.item-intro}

:::{.mt-8}
Beyond the model and route types, Ema leaves it up to you as to how to develop your site. The following are not *required* when using Ema; nevertheless they are useful inasmuch as they capture common patterns in writing a good static site:

* [Tailwind + Blaze](guide/tailwind.md) -- [We recommend--but not mandate--Tailwind for CSS and blaze-html as HTML DSL]{.item-intro}
* [Working with files](guide/filesystem.md) -- [Ema provides a helper to support hot-reload on files]{.item-intro}
* [Converting Markdown](guide/markdown.md) -- [Pointers on how to work with Markdown files]{.item-intro}