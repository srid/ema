-- | Helper to deal with slug trees
--
-- TODO: Move to a separate package
module Ema.Helper.PathTree where

import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Tree (Tree (Node))
import Data.Tree qualified as Tree

-- -------------------
-- Data.Tree helpers
-- -------------------

treeInsertPath :: Eq a => NonEmpty a -> [Tree a] -> [Tree a]
treeInsertPath =
  treeInsertPathMaintainingOrder void

-- | Insert a node by path into a tree with descendants that are ordered.
--
-- Insertion will guarantee that descendants continue to be ordered as expected.
--
-- The order of descendents is determined by the given order function, which
-- takes the path to a node and return that node's order. The intention is to
-- lookup the actual order value which exists *outside* of the tree
-- datastructure itself.
treeInsertPathMaintainingOrder :: (Eq a, Ord ord) => (NonEmpty a -> ord) -> NonEmpty a -> [Tree a] -> [Tree a]
treeInsertPathMaintainingOrder ordF path t =
  orderedTreeInsertPath ordF (toList path) t []
  where
    orderedTreeInsertPath :: (Eq a, Ord b) => (NonEmpty a -> b) -> [a] -> [Tree a] -> [a] -> [Tree a]
    orderedTreeInsertPath _ [] trees _ =
      trees
    orderedTreeInsertPath pathOrder (top : rest) trees ancestors =
      case treeFindChild top trees of
        Nothing ->
          let newChild = Node top $ orderedTreeInsertPath pathOrder rest [] (top : ancestors)
           in sortChildrenOn pathOrder (trees <> one newChild)
        Just (Node _match grandChildren) ->
          let oneDead = treeDeleteChild top trees
              newChild = Node top $ orderedTreeInsertPath pathOrder rest grandChildren (top : ancestors)
           in sortChildrenOn pathOrder (oneDead <> one newChild)
      where
        treeFindChild x xs =
          List.find (\n -> Tree.rootLabel n == x) xs
        sortChildrenOn f =
          sortOn $ (\s -> f $ NE.reverse $ s :| ancestors) . Tree.rootLabel

treeDeletePath :: Eq a => NonEmpty a -> [Tree a] -> [Tree a]
treeDeletePath =
  treeDeletePathWithLastBehavingAs $ \lastInPath ts ->
    List.deleteBy (\x y -> Tree.rootLabel x == Tree.rootLabel y) (Node lastInPath []) ts

treeDeleteLeafPath :: Eq a => NonEmpty a -> [Tree a] -> [Tree a]
treeDeleteLeafPath =
  treeDeletePathWithLastBehavingAs $ \lastInPath ts ->
    case ts of
      [t] -> [t | Tree.rootLabel t /= lastInPath]
      _ -> ts

treeDeletePathWithLastBehavingAs :: forall a. Eq a => (a -> [Tree a] -> [Tree a]) -> NonEmpty a -> [Tree a] -> [Tree a]
treeDeletePathWithLastBehavingAs f slugs =
  go (toList slugs)
  where
    go :: [a] -> [Tree a] -> [Tree a]
    go [] t = t
    go [p] ts =
      f p ts
    go (p : ps) t =
      t <&> \node@(Node x xs) ->
        if x == p
          then Node x $ go ps xs
          else node

treeDeleteChild :: Eq a => a -> [Tree a] -> [Tree a]
treeDeleteChild x =
  List.deleteBy (\p q -> Tree.rootLabel p == Tree.rootLabel q) (Node x [])
