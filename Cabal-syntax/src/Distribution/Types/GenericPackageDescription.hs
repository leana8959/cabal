{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.Types.GenericPackageDescription
  ( GenericPackageDescription
  , GenericPackageDescriptionBarbie (..)
  , emptyGenericPackageDescription
  ) where

import Distribution.Compat.Prelude
import Prelude ()

-- lens
import Distribution.Compat.Lens as L
import qualified Distribution.Types.BuildInfo.Lens as L

import Distribution.Types.PackageDescription

import Distribution.Package
import Distribution.Types.Benchmark
import Distribution.Types.CondTree
import Distribution.Types.ConfVar
import Distribution.Types.Executable
import Distribution.Types.Flag
import Distribution.Types.ForeignLib
import Distribution.Types.Library
import Distribution.Types.TestSuite
import Distribution.Types.UnqualComponentName
import Distribution.Version

import Data.Kind

data WithTrivia a = WithTrivia a

type family Modify (f :: Type -> Type) (a :: Type) where
  Modify Identity a = a
  -- A bad placeholder for Trivia
  Modify WithTrivia a = ([String], a)
  Modify _ a = a

-- ---------------------------------------------------------------------------
-- The 'GenericPackageDescription' type

type GenericPackageDescription = GenericPackageDescriptionBarbie Identity

data GenericPackageDescriptionBarbie (f :: Type -> Type) = GenericPackageDescription
  { packageDescription :: Modify f PackageDescription
  , gpdScannedVersion :: Modify f (Maybe Version)
  -- ^ This is a version as specified in source.
  --   We populate this field in index reading for dummy GPDs,
  --   only when GPD reading failed, but scanning haven't.
  --
  --   Cabal-the-library never produces GPDs with Just as gpdScannedVersion.
  --
  --   Perfectly, PackageIndex should have sum type, so we don't need to
  --   have dummy GPDs.
  , genPackageFlags :: Modify f [PackageFlag]
  , condLibrary :: Modify f (Maybe (CondTree ConfVar [Dependency] Library))
  , condSubLibraries
      :: Modify f [ ( UnqualComponentName , CondTree ConfVar [Dependency] Library) ]
  , condForeignLibs
      :: Modify f [ ( UnqualComponentName , CondTree ConfVar [Dependency] ForeignLib) ]
  , condExecutables
      :: Modify f [ ( UnqualComponentName , CondTree ConfVar [Dependency] Executable) ]
  , condTestSuites
      :: Modify f [ ( UnqualComponentName , CondTree ConfVar [Dependency] TestSuite) ]
  , condBenchmarks
      :: Modify f [ ( UnqualComponentName , CondTree ConfVar [Dependency] Benchmark) ]
  }

type AllGPDFields (c :: Type -> Constraint) (f :: Type -> Type) =
  ( c (Modify f PackageDescription)
  , c (Modify f (Maybe Version))
  , c (Modify f [PackageFlag])
  , c (Modify f (Maybe (CondTree ConfVar [Dependency] Library)))
  , c (Modify f [(UnqualComponentName, CondTree ConfVar [Dependency] Library)])
  , c (Modify f [(UnqualComponentName, CondTree ConfVar [Dependency] ForeignLib)])
  , c (Modify f [(UnqualComponentName, CondTree ConfVar [Dependency] Executable)])
  , c (Modify f [(UnqualComponentName, CondTree ConfVar [Dependency] TestSuite)])
  , c (Modify f [(UnqualComponentName, CondTree ConfVar [Dependency] Benchmark)])
  )

deriving instance forall (f :: Type -> Type)
   . AllGPDFields Eq f
  => Eq (GenericPackageDescriptionBarbie f)

deriving instance forall (f :: Type -> Type)
   . AllGPDFields Show f
  => Show (GenericPackageDescriptionBarbie f)

deriving instance forall (f :: Type -> Type)
   . ( Typeable f
     , AllGPDFields Data f
     )
  => Data (GenericPackageDescriptionBarbie f)

deriving instance forall (f :: Type -> Type)
   . AllGPDFields Generic f
  => Generic (GenericPackageDescriptionBarbie f)

instance Package (GenericPackageDescriptionBarbie Identity) where
  packageId = packageId . packageDescription

deriving anyclass instance forall (f :: Type -> Type)
   . ( AllGPDFields Binary f
     , AllGPDFields Generic f
     )
  => Binary (GenericPackageDescriptionBarbie f)

instance Structured GenericPackageDescription
instance NFData GenericPackageDescription where rnf = genericRnf

emptyGenericPackageDescription :: GenericPackageDescription
emptyGenericPackageDescription = GenericPackageDescription emptyPackageDescription Nothing [] Nothing [] [] [] [] []

-- -----------------------------------------------------------------------------
-- Traversal Instances

instance L.HasBuildInfos GenericPackageDescription where
  traverseBuildInfos f (GenericPackageDescription p v a1 x1 x2 x3 x4 x5 x6) =
    GenericPackageDescription
      <$> L.traverseBuildInfos f p
      <*> pure v
      <*> pure a1
      <*> (traverse . traverseCondTreeBuildInfo) f x1
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x2
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x3
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x4
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x5
      <*> (traverse . L._2 . traverseCondTreeBuildInfo) f x6

-- We use this traversal to keep [Dependency] field in CondTree up to date.
traverseCondTreeBuildInfo
  :: forall f comp v
   . (Applicative f, L.HasBuildInfo comp)
  => LensLike' f (CondTree v [Dependency] comp) L.BuildInfo
traverseCondTreeBuildInfo g = node
  where
    mkCondNode :: comp -> [CondBranch v [Dependency] comp] -> CondTree v [Dependency] comp
    mkCondNode comp = CondNode comp (view L.targetBuildDepends comp)

    node (CondNode comp _ branches) =
      mkCondNode
        <$> L.buildInfo g comp
        <*> traverse branch branches

    branch (CondBranch v x y) =
      CondBranch v
        <$> node x
        <*> traverse node y
