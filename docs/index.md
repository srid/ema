# Ema static site generator

:::{.avatar}
![](https://raw.githubusercontent.com/srid/ema/master/ema.svg)
:::

[Ema](https://github.com/srid/ema) is a next-gen **Haskell** library for building [jamstack-style](https://jamstack.org/) static sites. Ema sites are *change-aware*; in addition to good ol' static site generation, it provides a live server supporting **fast hot-reload** on the browser on code *or* data change. 

The ultimate goal of ema is to make it possible to easily implement your own [neuron](https://neuron.zettel.page/), or just about any app that creates a browser view of arbitrarily changing data (on disk, database, or whatever). Ema is designed to facilitate creation of apps whose data is normally *edited* via traditional mechanisms (eg: text editor) but *rendered* as a delightful web page - so as to provide an economical read-only view, of your data, on desktop & mobile.

* [Getting Started](getting-started.md)
* [How To](howto.md)
* [Features](features.md)

## Features

:::{.flex .flex-wrap .justify-evenly .items-center .bg-gray-100 .py-4 }

:::{.feature}
Hot Reload
:::

:::{.feature}
Server Side Rendering
:::

:::{.feature}
Haskell's safety
:::

:::