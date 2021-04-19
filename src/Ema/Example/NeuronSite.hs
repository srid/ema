{-# LANGUAGE TypeApplications #-}

-- | An advanced example demonstrating how to build something like neuron
module Ema.Example.NeuronSite where

{-
type Zk = Map FilePath ()

runNeuron :: FilePath -> IO ()
runNeuron fp = do
  s <- neuronModel fp
  runEma $
    Ema s $ \_zk () -> do
      "TODO"

neuronModel :: FilePath -> IO (Changing Zk)
neuronModel notebookDir = do
  ch <- watchDir notebookDir
  buildWorldIncrementally mempty ch $ \zk diff ->
    foldl' go zk (Map.toList diff)
  where
    watchDir :: FilePath -> IO (TBQueue (Map FilePath (Maybe ByteString)))
    watchDir = undefined

    buildWorldIncrementally ::
      state ->
      TBQueue worldChange ->
      (state -> worldChange -> state) ->
      IO (Changing state)
    buildWorldIncrementally _state0 _change _f =
      undefined
    go zk (k, mv) = case mv of
      Nothing ->
        -- Deleted!
        Map.delete k zk
      Just newVal ->
        Map.insert k (parseMarkdown newVal) zk
    parseMarkdown = const ()
-}
