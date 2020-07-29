{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_dolla_consensus_base (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/nhenin/dev/dolla/consensus-base/.stack-work/install/x86_64-osx/34b6ea020c962e891a0d814a0a60085efe9bbb501ac4a63341bd7b14ca09341f/8.8.3/bin"
libdir     = "/Users/nhenin/dev/dolla/consensus-base/.stack-work/install/x86_64-osx/34b6ea020c962e891a0d814a0a60085efe9bbb501ac4a63341bd7b14ca09341f/8.8.3/lib/x86_64-osx-ghc-8.8.3/dolla-consensus-base-1.0.0-9ma6K2Vfqs4M4Fb8yFvJg-test"
dynlibdir  = "/Users/nhenin/dev/dolla/consensus-base/.stack-work/install/x86_64-osx/34b6ea020c962e891a0d814a0a60085efe9bbb501ac4a63341bd7b14ca09341f/8.8.3/lib/x86_64-osx-ghc-8.8.3"
datadir    = "/Users/nhenin/dev/dolla/consensus-base/.stack-work/install/x86_64-osx/34b6ea020c962e891a0d814a0a60085efe9bbb501ac4a63341bd7b14ca09341f/8.8.3/share/x86_64-osx-ghc-8.8.3/dolla-consensus-base-1.0.0"
libexecdir = "/Users/nhenin/dev/dolla/consensus-base/.stack-work/install/x86_64-osx/34b6ea020c962e891a0d814a0a60085efe9bbb501ac4a63341bd7b14ca09341f/8.8.3/libexec/x86_64-osx-ghc-8.8.3/dolla-consensus-base-1.0.0"
sysconfdir = "/Users/nhenin/dev/dolla/consensus-base/.stack-work/install/x86_64-osx/34b6ea020c962e891a0d814a0a60085efe9bbb501ac4a63341bd7b14ca09341f/8.8.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "dolla_consensus_base_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "dolla_consensus_base_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "dolla_consensus_base_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "dolla_consensus_base_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "dolla_consensus_base_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "dolla_consensus_base_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
