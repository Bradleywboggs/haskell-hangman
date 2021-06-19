{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hangman_hangman_hangman (
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

bindir     = "/Users/bboggs/Developer/projects/haskell/projects/hangman/.stack-work/install/x86_64-osx/7adaeaca5c444f44075c1ff9f4492ec121dbd135e582709f9bf92e39ae46da6d/8.10.4/bin"
libdir     = "/Users/bboggs/Developer/projects/haskell/projects/hangman/.stack-work/install/x86_64-osx/7adaeaca5c444f44075c1ff9f4492ec121dbd135e582709f9bf92e39ae46da6d/8.10.4/lib/x86_64-osx-ghc-8.10.4/hangman-hangman-hangman-0.1.0.0-1MGxMBlkK85ADize9qZ1iQ-hangman-hangman-hangman"
dynlibdir  = "/Users/bboggs/Developer/projects/haskell/projects/hangman/.stack-work/install/x86_64-osx/7adaeaca5c444f44075c1ff9f4492ec121dbd135e582709f9bf92e39ae46da6d/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/bboggs/Developer/projects/haskell/projects/hangman/.stack-work/install/x86_64-osx/7adaeaca5c444f44075c1ff9f4492ec121dbd135e582709f9bf92e39ae46da6d/8.10.4/share/x86_64-osx-ghc-8.10.4/hangman-hangman-hangman-0.1.0.0"
libexecdir = "/Users/bboggs/Developer/projects/haskell/projects/hangman/.stack-work/install/x86_64-osx/7adaeaca5c444f44075c1ff9f4492ec121dbd135e582709f9bf92e39ae46da6d/8.10.4/libexec/x86_64-osx-ghc-8.10.4/hangman-hangman-hangman-0.1.0.0"
sysconfdir = "/Users/bboggs/Developer/projects/haskell/projects/hangman/.stack-work/install/x86_64-osx/7adaeaca5c444f44075c1ff9f4492ec121dbd135e582709f9bf92e39ae46da6d/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hangman_hangman_hangman_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hangman_hangman_hangman_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hangman_hangman_hangman_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hangman_hangman_hangman_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hangman_hangman_hangman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hangman_hangman_hangman_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
