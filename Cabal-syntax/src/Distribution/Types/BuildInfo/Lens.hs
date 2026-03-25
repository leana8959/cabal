{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Types.BuildInfo.Lens
  ( BuildInfo
  , HasBuildInfo (..)
  , HasBuildInfos (..)
  ) where

import Distribution.Compat.Lens
import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compiler (PerCompilerFlavor)
import Distribution.ModuleName (ModuleName)
import Distribution.Types.BuildInfo (BuildInfo, BuildInfoWith)
import Distribution.Types.Dependency (Dependency)
import Distribution.Types.ExeDependency (ExeDependency)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency)
import Distribution.Types.Mixin (Mixin)
import Distribution.Types.PkgconfigDependency (PkgconfigDependency)
import Distribution.Utils.Path
import Language.Haskell.Extension (Extension, Language)

import Data.Kind
import qualified Distribution.Types.Modify as Mod
import qualified Distribution.Types.BuildInfo as T

class HasBuildInfo mod a where
  buildInfo :: Lens' a (BuildInfoWith mod)

  buildable :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a Bool
  buildable = buildInfo @mod . buildable @mod

  buildTools :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [LegacyExeDependency]
  buildTools = buildInfo @mod . buildTools @mod

  buildToolDepends :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [ExeDependency]
  buildToolDepends = buildInfo @mod . buildToolDepends @mod

  cppOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  cppOptions = buildInfo @mod . cppOptions @mod

  asmOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  asmOptions = buildInfo @mod . asmOptions @mod

  cmmOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  cmmOptions = buildInfo @mod . cmmOptions @mod

  ccOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  ccOptions = buildInfo @mod . ccOptions @mod

  cxxOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  cxxOptions = buildInfo @mod . cxxOptions @mod

  jsppOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  jsppOptions = buildInfo @mod . jsppOptions @mod

  ldOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  ldOptions = buildInfo @mod . ldOptions @mod

  hsc2hsOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  hsc2hsOptions = buildInfo @mod . hsc2hsOptions @mod

  pkgconfigDepends :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [PkgconfigDependency]
  pkgconfigDepends = buildInfo @mod . pkgconfigDepends @mod

  frameworks :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [RelativePath Framework File]
  frameworks = buildInfo @mod . frameworks @mod

  extraFrameworkDirs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg (Dir Framework)]
  extraFrameworkDirs = buildInfo @mod . extraFrameworkDirs @mod

  asmSources :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg File]
  asmSources = buildInfo @mod . asmSources @mod

  cmmSources :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg File]
  cmmSources = buildInfo @mod . cmmSources @mod

  cSources :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg File]
  cSources = buildInfo @mod . cSources @mod

  cxxSources :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg File]
  cxxSources = buildInfo @mod . cxxSources @mod

  jsSources :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg File]
  jsSources = buildInfo @mod . jsSources @mod

  hsSourceDirs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg (Dir Source)]
  hsSourceDirs = buildInfo @mod . hsSourceDirs @mod

  otherModules :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [ModuleName]
  otherModules = buildInfo @mod . otherModules @mod

  virtualModules :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [ModuleName]
  virtualModules = buildInfo @mod . virtualModules @mod

  autogenModules :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [ModuleName]
  autogenModules = buildInfo @mod . autogenModules @mod

  defaultLanguage :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a (Maybe Language)
  defaultLanguage = buildInfo @mod . defaultLanguage @mod

  otherLanguages :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [Language]
  otherLanguages = buildInfo @mod . otherLanguages @mod

  defaultExtensions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [Extension]
  defaultExtensions = buildInfo @mod . defaultExtensions @mod

  otherExtensions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [Extension]
  otherExtensions = buildInfo @mod . otherExtensions @mod

  oldExtensions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [Extension]
  oldExtensions = buildInfo @mod . oldExtensions @mod

  extraLibs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  extraLibs = buildInfo @mod . extraLibs @mod

  extraLibsStatic :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  extraLibsStatic = buildInfo @mod . extraLibsStatic @mod

  extraGHCiLibs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  extraGHCiLibs = buildInfo @mod . extraGHCiLibs @mod

  extraBundledLibs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  extraBundledLibs = buildInfo @mod . extraBundledLibs @mod

  extraLibFlavours :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  extraLibFlavours = buildInfo @mod . extraLibFlavours @mod

  extraDynLibFlavours :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [String]
  extraDynLibFlavours = buildInfo @mod . extraDynLibFlavours @mod

  extraLibDirs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg (Dir Lib)]
  extraLibDirs = buildInfo @mod . extraLibDirs @mod

  extraLibDirsStatic :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg (Dir Lib)]
  extraLibDirsStatic = buildInfo @mod . extraLibDirsStatic @mod

  includeDirs :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Pkg (Dir Include)]
  includeDirs = buildInfo @mod . includeDirs @mod

  includes :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [SymbolicPath Include File]
  includes = buildInfo @mod . includes @mod

  autogenIncludes :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [RelativePath Include File]
  autogenIncludes = buildInfo @mod . autogenIncludes @mod

  installIncludes :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [RelativePath Include File]
  installIncludes = buildInfo @mod . installIncludes @mod

  options :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a (PerCompilerFlavor [String])
  options = buildInfo @mod . options @mod

  profOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a (PerCompilerFlavor [String])
  profOptions = buildInfo @mod . profOptions @mod

  sharedOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a (PerCompilerFlavor [String])
  sharedOptions = buildInfo @mod . sharedOptions @mod

  profSharedOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a (PerCompilerFlavor [String])
  profSharedOptions = buildInfo @mod . profSharedOptions @mod

  staticOptions :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a (PerCompilerFlavor [String])
  staticOptions = buildInfo @mod . staticOptions @mod

  customFieldsBI :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [(String, String)]
  customFieldsBI = buildInfo @mod . customFieldsBI @mod

  targetBuildDepends :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [Dependency]
  targetBuildDepends = buildInfo @mod . targetBuildDepends @mod

  mixins :: (HasBuildInfo mod (BuildInfoWith mod)) => Lens' a [Mixin]
  mixins = buildInfo @mod . mixins @mod

instance HasBuildInfo Mod.Bare (BuildInfoWith Mod.Bare) where
  buildInfo = id
  {-# INLINE buildInfo #-}

  buildable f s = fmap (\x -> s{T.buildable = x}) (f (T.buildable s))
  {-# INLINE buildable #-}

  buildTools f s = fmap (\x -> s{T.buildTools = x}) (f (T.buildTools s))
  {-# INLINE buildTools #-}

  buildToolDepends f s = fmap (\x -> s{T.buildToolDepends = x}) (f (T.buildToolDepends s))
  {-# INLINE buildToolDepends #-}

  cppOptions f s = fmap (\x -> s{T.cppOptions = x}) (f (T.cppOptions s))
  {-# INLINE cppOptions #-}

  asmOptions f s = fmap (\x -> s{T.asmOptions = x}) (f (T.asmOptions s))
  {-# INLINE asmOptions #-}

  cmmOptions f s = fmap (\x -> s{T.cmmOptions = x}) (f (T.cmmOptions s))
  {-# INLINE cmmOptions #-}

  ccOptions f s = fmap (\x -> s{T.ccOptions = x}) (f (T.ccOptions s))
  {-# INLINE ccOptions #-}

  cxxOptions f s = fmap (\x -> s{T.cxxOptions = x}) (f (T.cxxOptions s))
  {-# INLINE cxxOptions #-}

  jsppOptions f s = fmap (\x -> s{T.jsppOptions = x}) (f (T.jsppOptions s))
  {-# INLINE jsppOptions #-}

  ldOptions f s = fmap (\x -> s{T.ldOptions = x}) (f (T.ldOptions s))
  {-# INLINE ldOptions #-}

  hsc2hsOptions f s = fmap (\x -> s{T.hsc2hsOptions = x}) (f (T.hsc2hsOptions s))
  {-# INLINE hsc2hsOptions #-}

  pkgconfigDepends f s = fmap (\x -> s{T.pkgconfigDepends = x}) (f (T.pkgconfigDepends s))
  {-# INLINE pkgconfigDepends #-}

  frameworks f s = fmap (\x -> s{T.frameworks = x}) (f (T.frameworks s))
  {-# INLINE frameworks #-}

  extraFrameworkDirs f s = fmap (\x -> s{T.extraFrameworkDirs = x}) (f (T.extraFrameworkDirs s))
  {-# INLINE extraFrameworkDirs #-}

  asmSources f s = fmap (\x -> s{T.asmSources = x}) (f (T.asmSources s))
  {-# INLINE asmSources #-}

  cmmSources f s = fmap (\x -> s{T.cmmSources = x}) (f (T.cmmSources s))
  {-# INLINE cmmSources #-}

  cSources f s = fmap (\x -> s{T.cSources = x}) (f (T.cSources s))
  {-# INLINE cSources #-}

  cxxSources f s = fmap (\x -> s{T.cxxSources = x}) (f (T.cxxSources s))
  {-# INLINE cxxSources #-}

  jsSources f s = fmap (\x -> s{T.jsSources = x}) (f (T.jsSources s))
  {-# INLINE jsSources #-}

  hsSourceDirs f s = fmap (\x -> s{T.hsSourceDirs = x}) (f (T.hsSourceDirs s))
  {-# INLINE hsSourceDirs #-}

  otherModules f s = fmap (\x -> s{T.otherModules = x}) (f (T.otherModules s))
  {-# INLINE otherModules #-}

  virtualModules f s = fmap (\x -> s{T.virtualModules = x}) (f (T.virtualModules s))
  {-# INLINE virtualModules #-}

  autogenModules f s = fmap (\x -> s{T.autogenModules = x}) (f (T.autogenModules s))
  {-# INLINE autogenModules #-}

  defaultLanguage f s = fmap (\x -> s{T.defaultLanguage = x}) (f (T.defaultLanguage s))
  {-# INLINE defaultLanguage #-}

  otherLanguages f s = fmap (\x -> s{T.otherLanguages = x}) (f (T.otherLanguages s))
  {-# INLINE otherLanguages #-}

  defaultExtensions f s = fmap (\x -> s{T.defaultExtensions = x}) (f (T.defaultExtensions s))
  {-# INLINE defaultExtensions #-}

  otherExtensions f s = fmap (\x -> s{T.otherExtensions = x}) (f (T.otherExtensions s))
  {-# INLINE otherExtensions #-}

  oldExtensions f s = fmap (\x -> s{T.oldExtensions = x}) (f (T.oldExtensions s))
  {-# INLINE oldExtensions #-}

  extraLibs f s = fmap (\x -> s{T.extraLibs = x}) (f (T.extraLibs s))
  {-# INLINE extraLibs #-}

  extraLibsStatic f s = fmap (\x -> s{T.extraLibsStatic = x}) (f (T.extraLibsStatic s))
  {-# INLINE extraLibsStatic #-}

  extraGHCiLibs f s = fmap (\x -> s{T.extraGHCiLibs = x}) (f (T.extraGHCiLibs s))
  {-# INLINE extraGHCiLibs #-}

  extraBundledLibs f s = fmap (\x -> s{T.extraBundledLibs = x}) (f (T.extraBundledLibs s))
  {-# INLINE extraBundledLibs #-}

  extraLibFlavours f s = fmap (\x -> s{T.extraLibFlavours = x}) (f (T.extraLibFlavours s))
  {-# INLINE extraLibFlavours #-}

  extraDynLibFlavours f s = fmap (\x -> s{T.extraDynLibFlavours = x}) (f (T.extraDynLibFlavours s))
  {-# INLINE extraDynLibFlavours #-}

  extraLibDirs f s = fmap (\x -> s{T.extraLibDirs = x}) (f (T.extraLibDirs s))
  {-# INLINE extraLibDirs #-}

  extraLibDirsStatic f s = fmap (\x -> s{T.extraLibDirsStatic = x}) (f (T.extraLibDirsStatic s))
  {-# INLINE extraLibDirsStatic #-}

  includeDirs f s = fmap (\x -> s{T.includeDirs = x}) (f (T.includeDirs s))
  {-# INLINE includeDirs #-}

  includes f s = fmap (\x -> s{T.includes = x}) (f (T.includes s))
  {-# INLINE includes #-}

  autogenIncludes f s = fmap (\x -> s{T.autogenIncludes = x}) (f (T.autogenIncludes s))
  {-# INLINE autogenIncludes #-}

  installIncludes f s = fmap (\x -> s{T.installIncludes = x}) (f (T.installIncludes s))
  {-# INLINE installIncludes #-}

  options f s = fmap (\x -> s{T.options = x}) (f (T.options s))
  {-# INLINE options #-}

  profOptions f s = fmap (\x -> s{T.profOptions = x}) (f (T.profOptions s))
  {-# INLINE profOptions #-}

  sharedOptions f s = fmap (\x -> s{T.sharedOptions = x}) (f (T.sharedOptions s))
  {-# INLINE sharedOptions #-}

  profSharedOptions f s = fmap (\x -> s{T.profSharedOptions = x}) (f (T.profSharedOptions s))
  {-# INLINE profSharedOptions #-}

  staticOptions f s = fmap (\x -> s{T.staticOptions = x}) (f (T.staticOptions s))
  {-# INLINE staticOptions #-}

  customFieldsBI f s = fmap (\x -> s{T.customFieldsBI = x}) (f (T.customFieldsBI s))
  {-# INLINE customFieldsBI #-}

  targetBuildDepends f s = fmap (\x -> s{T.targetBuildDepends = x}) (f (T.targetBuildDepends s))
  {-# INLINE targetBuildDepends #-}

  mixins f s = fmap (\x -> s{T.mixins = x}) (f (T.mixins s))
  {-# INLINE mixins #-}

class HasBuildInfos mod a where
  traverseBuildInfos :: Traversal' a (BuildInfoWith mod)
