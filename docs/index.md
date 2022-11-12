---
page:
  description: | 
    Build jamstack-style static sites in Haskell, with a live server supporting fast hot-reload in the browser on code or data change.
---
# Ema

:::{.avatar .w-32 .h-32 .float-right}
![[favicon.svg]]
:::

{.text-xl .mb-8}
[Ema](https://github.com/srid/ema) is a next-gen library for building [jamstack-style](https://jamstack.org/) static sites in [**Haskell**](https://www.srid.ca/haskell). Ema sites are *change-aware*; in addition to good ol' static site generation, Ema provides a [[live-server|live server]] supporting **fast hot-reload** in the browser, on code *or* data change. 

{.text-gray-600} 
The ultimate purpose of ema is to facilitate creating with ease your own [neuron](https://neuron.zettel.page/)[^emanote], or just about any app that creates a _browser view of arbitrarily changing data_ (on disk, database, or whatever). In Ema apps, data is _edited_ via traditional mechanisms (e.g., text editor), and its view is _rendered_ as a delightful web page, thus economically providing a read-only view of your data on desktop & mobile. A classic static site is just one type of such [*kind*]{title="See what we did there?"} of apps. 

#### [[start|Get started]]{.flex .justify-center .items-center .my-8}

:::{.flex .justify-center .items-center .mb-8}
<video autoplay="" loop="" muted="">
  <source src="static/ema-demo.mp4" />
  <p>Your browser doesn't support HTML5 video. Here is a <a href="static/ema-demo.mp4">link to the video</a> instead.</p>
</video>
:::

[^emanote]: This ultimate purpose is already realized in regards to neuron; see [Emanote](https://emanote.srid.ca/), which is built on top of Ema.
