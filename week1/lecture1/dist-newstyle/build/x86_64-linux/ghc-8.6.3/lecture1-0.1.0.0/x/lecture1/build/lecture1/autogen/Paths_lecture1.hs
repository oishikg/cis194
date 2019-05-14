{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_lecture1 (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/oishik/.cabal/bin"
libdir     = "/home/oishik/.cabal/lib/x86_64-linux-ghc-8.6.3/lecture1-0.1.0.0-inplace-lecture1"
dynlibdir  = "/home/oishik/.cabal/lib/x86_64-linux-ghc-8.6.3"
datadir    = "/home/oishik/.cabal/share/x86_64-linux-ghc-8.6.3/lecture1-0.1.0.0"
libexecdir = "/home/oishik/.cabal/libexec/x86_64-linux-ghc-8.6.3/lecture1-0.1.0.0"
sysconfdir = "/home/oishik/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lecture1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lecture1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lecture1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lecture1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lecture1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lecture1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
