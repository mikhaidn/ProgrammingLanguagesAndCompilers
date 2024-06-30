{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp4_forth (
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

bindir     = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp4-forth/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/bin"
libdir     = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp4-forth/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/lib/aarch64-osx-ghc-8.10.7/mp4-forth-0.1.0.0-CsGFQzDKzHj3hPLqpU6lJk-mp4"
dynlibdir  = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp4-forth/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/lib/aarch64-osx-ghc-8.10.7"
datadir    = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp4-forth/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/share/aarch64-osx-ghc-8.10.7/mp4-forth-0.1.0.0"
libexecdir = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp4-forth/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/libexec/aarch64-osx-ghc-8.10.7/mp4-forth-0.1.0.0"
sysconfdir = "/Users/danmikhail/git/ProgrammingLanguagesAndCompilers/mp4-forth/.stack-work/install/aarch64-osx/75a682f6f2fa22269098ed73fc07efe5d8a0059ad58ca9ceb4aa12f3449b8bf3/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp4_forth_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp4_forth_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp4_forth_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp4_forth_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp4_forth_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp4_forth_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
