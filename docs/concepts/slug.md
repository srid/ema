---
order: 3
---
# Slug

:::{.note}
Note that the initial version of Ema used Slug in its routing system, but not anymore in the latest version. The Slug type is still left in the library for using as a helper.
:::

A slug is a component of a URL or file path. In an _URL_ like `/foo/bar`, there are two _slugs_: "foo" and "bar". URLs (as well as filepaths) can therefore be represented as lists of slugs (`[Slug]`).

```haskell
import Ema (Slug)

type URL = [Slug]
```

Slugs are automatically [unicode normalized](https://www.unicode.org/faq/normalization.html) to NFC to ensure that route links work reliably regardless of the underlying representation of any non-ascii link text.

The functions `decodeSlug` and `encodeSlug` allow you to convert between URLs and slugs.
