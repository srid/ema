
# `unionmount`

[unionmount](https://github.com/srid/unionmount) is a Haskell library that wraps `fsnotify` to provide a [[dynamic]]-friendly API to listen on filesystem changes and update an in-memory [[model]]. The library interface is such that you can "mount" multiple directories ([union](https://en.wikipedia.org/wiki/Union_mount)'ing them in the process), and have it update the model when files on any of those directories change.

[Emanote](https://emanote.srid.ca/) uses it to monitor Markdown files (and other files) in its layers.