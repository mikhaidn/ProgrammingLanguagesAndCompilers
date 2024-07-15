{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp5_scheme (
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
version = Version [0,2,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp5-scheme/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/bin"
libdir     = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp5-scheme/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/lib/aarch64-osx-ghc-8.10.7/mp5-scheme-0.2.0.0-8XjINX525zN3P9XtktX39G-mp5-scheme"
dynlibdir  = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp5-scheme/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp5-scheme/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/share/aarch64-osx-ghc-8.10.7/mp5-scheme-0.2.0.0"
libexecdir = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp5-scheme/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/libexec/aarch64-osx-ghc-8.10.7/mp5-scheme-0.2.0.0"
sysconfdir = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp5-scheme/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp5_scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp5_scheme_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp5_scheme_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp5_scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp5_scheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp5_scheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
