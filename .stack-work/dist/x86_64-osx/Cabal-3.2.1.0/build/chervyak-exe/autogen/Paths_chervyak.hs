{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_chervyak (
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

bindir     = "/Users/a.v.protasov/Desktop/chervyak/.stack-work/install/x86_64-osx/6e662ca00c2c519e9791fa8f55c152c19834a7ae4aed40e9b1588abf2fcfb7da/8.10.7/bin"
libdir     = "/Users/a.v.protasov/Desktop/chervyak/.stack-work/install/x86_64-osx/6e662ca00c2c519e9791fa8f55c152c19834a7ae4aed40e9b1588abf2fcfb7da/8.10.7/lib/x86_64-osx-ghc-8.10.7/chervyak-0.1.0.0-2aOtlC4GeY0LAbtMBeT4pd-chervyak-exe"
dynlibdir  = "/Users/a.v.protasov/Desktop/chervyak/.stack-work/install/x86_64-osx/6e662ca00c2c519e9791fa8f55c152c19834a7ae4aed40e9b1588abf2fcfb7da/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/a.v.protasov/Desktop/chervyak/.stack-work/install/x86_64-osx/6e662ca00c2c519e9791fa8f55c152c19834a7ae4aed40e9b1588abf2fcfb7da/8.10.7/share/x86_64-osx-ghc-8.10.7/chervyak-0.1.0.0"
libexecdir = "/Users/a.v.protasov/Desktop/chervyak/.stack-work/install/x86_64-osx/6e662ca00c2c519e9791fa8f55c152c19834a7ae4aed40e9b1588abf2fcfb7da/8.10.7/libexec/x86_64-osx-ghc-8.10.7/chervyak-0.1.0.0"
sysconfdir = "/Users/a.v.protasov/Desktop/chervyak/.stack-work/install/x86_64-osx/6e662ca00c2c519e9791fa8f55c152c19834a7ae4aed40e9b1588abf2fcfb7da/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chervyak_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chervyak_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chervyak_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chervyak_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chervyak_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chervyak_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
