{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.PackageName
  ( PackageName
  , PackageNameAnn
  , PackageNameWith (..)
  , unannotatePackageName
  , unPackageName
  , mkPackageName
  , unPackageNameST
  , mkPackageNameST
  ) where

import Distribution.Compat.Prelude
import Distribution.Utils.ShortText
import Prelude ()

import Distribution.Parsec
import Distribution.Trivia
import Distribution.Pretty
import qualified Text.PrettyPrint as Disp

import qualified Distribution.Types.Modify as Mod
import Data.Kind

-- | A package name.
--
-- Use 'mkPackageName' and 'unPackageName' to convert from/to a
-- 'String'.
--
-- This type is opaque since @Cabal-2.0@
--
-- @since 2.0.0.2
type PackageName = PackageNameWith Mod.Bare
type PackageNameAnn = PackageNameWith Mod.Ann

type family ModifyPackageName (m :: Type) (a :: Type) where
  ModifyPackageName Mod.Bare a = a
  ModifyPackageName Mod.Ann a = Ann a

newtype PackageNameWith (m :: Type) = PackageName (ModifyPackageName m ShortText)
  deriving (Generic)

deriving instance Show PackageName
deriving instance Read PackageName
deriving instance Eq PackageName
deriving instance Ord PackageName
deriving instance Data PackageName

deriving instance Show PackageNameAnn
deriving instance Read PackageNameAnn
deriving instance Eq PackageNameAnn
deriving instance Ord PackageNameAnn
deriving instance Data PackageNameAnn

unannotatePackageName :: PackageNameWith Mod.Ann -> PackageName
unannotatePackageName (PackageName pname) = PackageName (unAnn pname)

-- | Convert 'PackageName' to 'String'
unPackageName :: PackageName -> String
unPackageName (PackageName s) = fromShortText s

-- | @since 3.4.0.0
unPackageNameST :: PackageName -> ShortText
unPackageNameST (PackageName s) = s

-- | Construct a 'PackageName' from a 'String'
--
-- 'mkPackageName' is the inverse to 'unPackageName'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 2.0.0.2
mkPackageName :: String -> PackageName
mkPackageName = PackageName . toShortText

-- | Construct a 'PackageName' from a 'ShortText'
--
-- Note: No validations are performed to ensure that the resulting
-- 'PackageName' is valid
--
-- @since 3.4.0.0
mkPackageNameST :: ShortText -> PackageName
mkPackageNameST = PackageName

-- | 'mkPackageName'
--
-- @since 2.0.0.2
instance IsString PackageName where
  fromString = mkPackageName

instance Binary PackageName
instance Structured PackageName

instance Pretty PackageName where
  pretty = Disp.text . unPackageName

instance Parsec PackageName where
  parsec = mkPackageName <$> parsecUnqualComponentName

instance Parsec (PackageNameWith Mod.Ann) where
  parsec =
    PackageName . Ann (ExactRepresentation "packagename trivia") . toShortText
      <$> parsecUnqualComponentName

instance NFData PackageName where
  rnf (PackageName pkg) = rnf pkg
