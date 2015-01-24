{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- TODO
module System.Environment.RelativeDataFiles
    (
    -- $intro
      getBaseDir
    , getBinDir
    , getDataDir
    , getSysconfigDir
    , getLibDir
    , getLibexecDir

    , getDataFileName
    )
  where

import Control.Exception (IOException, catch)
import Control.Monad (Monad(return))
import Data.Function ((.), ($), const)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import Data.Tuple (fst)
import System.Environment (getEnv)
import System.IO (IO, FilePath)

import System.FilePath ((</>), dropFileName, dropTrailingPathSeparator)

import System.Environment.Executable (splitExecutablePath)


-- | Return \"bin\" directory.
--
-- Algorithm is as follows:
--
-- 1. Check if environment variable @data_files_bindir@ is defined. If it is,
--    then return its value.
--
-- 2. Check if environment variable @data_files_basedir@ is defined. If it is,
--    then return its value with @('</>' \"bin\")@ appended to it.
--
-- 3. Get file path of currently executed program and return directory where it
--    resides.
getBinDir :: IO FilePath
getBinDir = do
    maybeBinDir <- getEnv' "data_files_bindir"
    case maybeBinDir of
        Just binDir -> return binDir
        Nothing     -> do
            maybeBaseDir <- getEnv' "data_files_basedir"
            case maybeBaseDir of
                Just baseDir -> return $ baseDir </> "bin"
                Nothing      ->
                    dropTrailingPathSeparator . fst <$> splitExecutablePath

-- | Return base direcotory where application is installed.
--
-- Algorithm is as follows:
--
-- 1. Check if environment variable @data_files_basedir@ is defined. If it is,
--    then return its value.
--
-- 2. Call 'getBaseDir' and drop last directory element (\"bin\").
getBaseDir :: IO FilePath
getBaseDir = do
    maybeBaseDir <- getEnv' "data_files_basedir"
    case maybeBaseDir of
        Just baseDir -> return baseDir
        Nothing      -> dropTrailingPathSeparator . dropFileName <$> getBinDir

-- | Utility function that is used for implementing a lot of other functions
-- in this module. It is not exported.
stdGetDir
    :: String
    -- ^ Environment variable name to check for its value.
    -> FilePath
    -- ^ Subdirectory of base directory. See 'getBaseDir' for details.
    -> IO FilePath
    -- ^ Absolute file path.
stdGetDir envVarName subdir = do
    maybeDir <- getEnv' envVarName
    case maybeDir of
        Just dir -> return dir
        Nothing  -> (</> subdir) <$> getBaseDir

getLibDir :: IO FilePath
getLibDir = stdGetDir "data_files_libdir" "lib"

getDataDir :: IO FilePath
getDataDir = stdGetDir "data_files_datadir" "share"

getSysconfigDir :: IO FilePath
getSysconfigDir = stdGetDir "data_files_sysconfdir" "etc"

getLibexecDir :: IO FilePath
getLibexecDir = stdGetDir "data_files_libexecdir" "libexec"

getDataFileName :: FilePath -> IO FilePath
getDataFileName relativeFilePath = (</> relativeFilePath) <$> getDataDir

-- | Utility function that returns 'Nothing' if environment variable is not
-- defined. Otherwise it behaves as 'getEnv'.
getEnv'
    :: String
    -- ^ Environment variable name.
    -> IO (Maybe String)
getEnv' var = (Just <$> getEnv var)
    `catch` forget ioException (return Nothing)
  where
    ioException = Proxy :: Proxy IOException

    forget :: Proxy b -> a -> b -> a
    forget Proxy = const

-- $intro
--
-- Expected directory layout:
--
-- > ${base_dir}
-- > |-- bin/${executable}
-- > |-- etc/
-- > |-- lib/
-- > |-- libexec/
-- > `-- share/
