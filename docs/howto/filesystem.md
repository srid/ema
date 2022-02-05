---
order: 2
---
# Working with files

If your static site is generated depending on local files on disk, the general flow of things is as follows:

```haskell
runEma render $ \model -> do
  -- Load everything on launch
  initialModel <- loadFilesAndBuildModel
  LVar.set model initialModel
  -- Continue to monitor and update the model
  observeFileSystem $ \action -> 
    LVar.modify model $ applyAction action
```

For monitoring local files on disk you would typically use something like [fsnotify](https://hackage.haskell.org/package/fsnotify) in place of `observeFileSystem`. What is the point of doing this? To support [hot reload](concepts/hot-reload.md) on _data_ change. Imagine that your static site is generated based on Markdown files as well as HTML templates on disk. If either the Markdown file, or a HTML template file is modified, we want the web browser to hot reload the updated HTML *instantly*. This is enabled by storing both these kinds of files in the application [model](guide/model.md) and using [LVar](concepts/lvar.md) to update it *over time*.

Much of this is provided by the `System.UnionMount` module from [unionmount](https://hackage.haskell.org/package/unionmount) package. You should use it:

```haskell
import qualified System.UnionMount as UnionMount

Ema.runEma render $ \model -> do
  let pats = [((), "**/*.md")]
      ignorePats = [".*"]
  void . UnionMount.mountOnLVar "." pats ignorePats model model0 $ \() fp action -> do
    case action of
      UnionMount.Refresh _ () -> do
        when (takeExtension fp == ".md") $ do
          log $ "Update: " <> fp 
          s <- readFileText fp
          pure $ Map.insert fp s
      UnionMount.Delete ->
        whenJust (takeExtension fp == ".md") $ do
          log $ "Delete: " <> fp
          pure $ Map.delete fp
```

`mountOnLVar` "mounts" the files you specify onto the [model LVar](concepts/lvar.md) such that any changes to them are *automatically* reflected in your [model](guide/model.md) value.

[Full example here](https://github.com/srid/ema-template/blob/master/src/Main.hs).
