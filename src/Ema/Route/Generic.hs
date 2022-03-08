{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ema.Route.Generic
  ( gMkRouteEncoder,
  )
where

import Data.List ((!!))
import Data.SOP.Constraint (SListIN)
import Data.Text qualified as T
import Ema.Route.Encoder
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Generics.SOP
import System.FilePath
  ( joinPath,
    splitDirectories,
    (</>),
  )
import Prelude hiding (All, Generic)

gMkRouteEncoder ::
  forall r.
  (Generic r, All2 IsRouteUnit (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) =>
  RouteEncoder () r
gMkRouteEncoder =
  unsafeMkRouteEncoder (const gEncodeRoute) (const gDecodeRoute) (const all_)
  where
    all_ :: [r]
    all_ = [] -- TODO

gEncodeRoute ::
  forall r.
  (Generic r, All2 IsRouteUnit (Code r), All IsRouteProd (Code r), HasDatatypeInfo r) =>
  r ->
  FilePath
gEncodeRoute x' =
  let x = unSOP $ from @r x'
      ctorNames :: [ConstructorName] =
        hcollapse $ hmap (K . constructorName) $ datatypeCtors @r
      ctorSuffix = ctorStripPrefix @r (ctorNames !! hindex x)
   in case hcollapse $ hcmap (Proxy @IsRouteProd) encProd x of
        Nothing -> ctorSuffix <> ".html"
        Just p -> ctorSuffix </> p
  where
    encProd :: forall xs. (IsRouteProd xs) => NP I xs -> K (Maybe FilePath) xs
    encProd =
      K . hcollapseMaybe . hcmap (Proxy @IsRouteUnit) encTerm
    encTerm :: forall b. IsRouteUnit b => I b -> K FilePath b
    encTerm =
      K . encodeRoute (mkRouteEncoder @b) () . unI

-- | Like `HCollapse`, but limited to 0 or 1 products in a n-ary structure.
class HCollapseMaybe h xs where
  hcollapseMaybe :: SListIN h xs => h (K a) xs -> Maybe a

instance HCollapseMaybe NP '[] where
  hcollapseMaybe Nil = Nothing

instance HCollapseMaybe NP '[p] where
  hcollapseMaybe (K x :* Nil) = Just x

instance (ps ~ TypeError ('Text "Expected at most 1 product")) => HCollapseMaybe NP (p ': p1 ': ps) where
  hcollapseMaybe _ = Nothing -- Unreachable, due to TypeError

class (IsRoute a, RouteModel a ~ ()) => IsRouteUnit a

instance (IsRoute a, RouteModel a ~ ()) => IsRouteUnit a

class (All IsRouteUnit xs, HCollapseMaybe NP xs) => IsRouteProd xs

instance (All IsRouteUnit xs, HCollapseMaybe NP xs) => IsRouteProd xs

datatypeCtors :: forall a. HasDatatypeInfo a => NP ConstructorInfo (Code a)
datatypeCtors = constructorInfo $ datatypeInfo (Proxy @a)

ctorStripPrefix :: forall a. HasDatatypeInfo a => ConstructorName -> String
ctorStripPrefix ctorName =
  let name = datatypeName $ datatypeInfo (Proxy @a)
   in maybe (error "ctor: bad naming") (T.unpack . T.toLower) $
        T.stripPrefix (T.pack $ name <> "_") (T.pack ctorName)

gDecodeRoute :: forall r. (Generic r, All IsRouteProd (Code r), All2 IsRouteUnit (Code r), HasDatatypeInfo r) => FilePath -> Maybe r
gDecodeRoute fp = do
  basePath : restPath <- pure $ splitDirectories fp
  -- Build the sum using an anamorphism
  to . SOP
    <$> mcana_NS @IsRouteProd @_ @_ @(NP I)
      Proxy
      (anamorphismSum basePath restPath)
      (datatypeCtors @r)
  where
    anamorphismSum :: forall xs xss. IsRouteProd xs => FilePath -> [FilePath] -> NP ConstructorInfo (xs ': xss) -> Either (Maybe (NP I xs)) (NP ConstructorInfo xss)
    anamorphismSum base rest (p :* ps) =
      fromMaybe (Right ps) $ do
        let ctorSuffix = ctorStripPrefix @r (constructorName p)
        Left <$> case sList @xs of
          SNil -> do
            -- Constructor without arguments
            guard $ ctorSuffix <> ".html" == base && null rest
            pure $ Just Nil
          SCons -> do
            -- Constructor with an argument
            guard $ ctorSuffix == base
            pure $
              mcana_NP @_ @_ @_ @I
                (Proxy @IsRouteUnit)
                anamorphismProduct
                Proxy
      where
        anamorphismProduct :: forall y1 ys1. (IsRouteUnit y1, SListI ys1) => Proxy (y1 ': ys1) -> Maybe (I y1, Proxy ys1)
        anamorphismProduct Proxy = case sList @ys1 of
          SNil -> do
            -- Recurse into the only product argument
            guard $ not $ null rest
            r' <- decodeRoute (mkRouteEncoder @y1) () $ joinPath rest
            pure (I r', Proxy)
          SCons ->
            -- Not reachable, due to HCollapseMaybe constraint
            Nothing

-- | Like `mcana_NS` but returns a Maybe
mcana_NS ::
  forall c proxy s f xs.
  (All c xs) =>
  proxy c ->
  (forall y ys. c y => s (y ': ys) -> Either (Maybe (f y)) (s ys)) ->
  s xs ->
  Maybe (NS f xs)
mcana_NS _ decide = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NS f ys)
    go SNil _ = Nothing
    go SCons s = case decide s of
      Left x -> Z <$> x
      Right s' -> S <$> go sList s'

-- | Like `cana_NP` but returns a Maybe
mcana_NP ::
  forall c proxy s f xs.
  (All c xs) =>
  proxy c ->
  (forall y ys. (c y, SListI ys) => s (y ': ys) -> Maybe (f y, s ys)) ->
  s xs ->
  Maybe (NP f xs)
mcana_NP _ uncons = go sList
  where
    go :: forall ys. (All c ys) => SList ys -> s ys -> Maybe (NP f ys)
    go SNil _ = pure Nil
    go SCons s = do
      (x, s') <- uncons s
      xs <- go sList s'
      pure $ x :* xs
