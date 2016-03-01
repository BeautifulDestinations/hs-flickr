module Paths_flickr (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,3,3], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/thomasd/Documents/github/BeautifulDestinations/.stack-work/install/x86_64-linux/lts-2.22/7.8.4/bin"
libdir     = "/home/thomasd/Documents/github/BeautifulDestinations/.stack-work/install/x86_64-linux/lts-2.22/7.8.4/lib/x86_64-linux-ghc-7.8.4/flickr-0.3.3"
datadir    = "/home/thomasd/Documents/github/BeautifulDestinations/.stack-work/install/x86_64-linux/lts-2.22/7.8.4/share/x86_64-linux-ghc-7.8.4/flickr-0.3.3"
libexecdir = "/home/thomasd/Documents/github/BeautifulDestinations/.stack-work/install/x86_64-linux/lts-2.22/7.8.4/libexec"
sysconfdir = "/home/thomasd/Documents/github/BeautifulDestinations/.stack-work/install/x86_64-linux/lts-2.22/7.8.4/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "flickr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "flickr_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "flickr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "flickr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "flickr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
