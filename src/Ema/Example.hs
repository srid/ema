{-# LANGUAGE TypeApplications #-}

module Ema.Example where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM.TVar (swapTVar)
import Data.List ((!!))
import qualified Data.Text as T
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
  )
import Ema.App (Changing (Changing), Ema (Ema), runEma, runEmaPure)
import qualified Ema.Layout as Layout
import Ema.Route (IsRoute (..), Slug (unSlug))
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- First, the simplest example using @runEmaPure@
-- ----------------------------------------------

runHelloWorld :: IO ()
runHelloWorld = do
  let name :: Text = "Srid"
  runEmaPure $ \() ->
    encodeUtf8 $ "Hello, " <> name

-- Next, an example involving routes & impure data (current time) using @runEma@
-- -----------------------------------------------------------------------------

data PeopleRoute
  = PR_Index
  | PR_Person Text
  deriving (Show)

instance IsRoute PeopleRoute where
  toSlug = \case
    PR_Index -> mempty
    PR_Person name -> one $ fromString . toString $ T.replace " " "_" name
  fromSlug = \case
    [] -> Just PR_Index
    [person] -> Just $ PR_Person (T.replace "_" " " $ unSlug person)
    _ -> Nothing

timeC :: IO (Changing UTCTime, IO ())
timeC = do
  var <- newTVarIO =<< getCurrentTime
  ch <- newEmptyTMVarIO
  let run = void $
        forever $ do
          now <- getCurrentTime
          atomically $ do
            void $ swapTVar var now
            void $ tryPutTMVar ch ()
          threadDelay $ 1 * 1000000
  pure (Changing var ch, run)

runSimpleSitePure :: IO ()
runSimpleSitePure = do
  (model, runTimeC) <- timeC
  race_ runTimeC (runEma $ Ema model render)
  where
    render now r =
      Layout.tailwindSite (H.title "Simple Site") $
        H.div ! A.class_ "container mx-auto" $ do
          H.header ! A.class_ "text-4xl font-bold border-b-1" $ "Simple Site!"
          case r of
            PR_Index -> do
              H.p ! A.style "color: red; text-3xl" $ "Checkout some profiles:"
              forM_ ["Srid Ratna", "ema", "Great India"] $ \person ->
                H.li $
                  routeElem (PR_Person person) $
                    H.toMarkup person
            PR_Person name -> do
              H.header ! A.class_ "text-2xl" $ H.toMarkup $ "Profile of " <> name
              H.div $ routeElem PR_Index "Go to Home"
          H.footer ! A.class_ "border-t-1 p-2 text-center" $ do
            "The current time is: "
            H.pre ! A.class_ "text-4xl" $ do
              let epoch = fromMaybe 0 . readMaybe @Int $ formatTime defaultTimeLocale "%s" now
                  colors = ["green", "purple", "red", "blue"]
                  color = colors !! mod epoch (length colors)
                  cls = "text-" <> color <> "-500"
              H.span ! A.class_ cls $ H.toMarkup $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S" now
    routeElem r w =
      H.a ! A.class_ "text-xl text-purple-500 hover:underline" ! routeHref r $ w
    routeHref r =
      A.href (fromString . toString $ routeUrl r)

-- Finally, an advanced example demonstrating how to build something like neuron
-- -----------------------------------------------------------------------------

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
