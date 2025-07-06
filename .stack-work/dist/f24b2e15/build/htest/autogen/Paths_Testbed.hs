{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_Testbed (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "E:\\Haskell\\Testbed\\.stack-work\\install\\826e91a6\\bin"
libdir     = "E:\\Haskell\\Testbed\\.stack-work\\install\\826e91a6\\lib\\x86_64-windows-ghc-9.8.4\\Testbed-0.1.0.0-DQ9oCn7V6ebAPp3vaaw56M-htest"
dynlibdir  = "E:\\Haskell\\Testbed\\.stack-work\\install\\826e91a6\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "E:\\Haskell\\Testbed\\.stack-work\\install\\826e91a6\\share\\x86_64-windows-ghc-9.8.4\\Testbed-0.1.0.0"
libexecdir = "E:\\Haskell\\Testbed\\.stack-work\\install\\826e91a6\\libexec\\x86_64-windows-ghc-9.8.4\\Testbed-0.1.0.0"
sysconfdir = "E:\\Haskell\\Testbed\\.stack-work\\install\\826e91a6\\etc"

getBinDir     = catchIO (getEnv "Testbed_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "Testbed_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "Testbed_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "Testbed_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Testbed_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Testbed_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
