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
module System.Environment.InstallPaths
    (
    -- * Environment Variables and Directory Layout
    -- $directoryLayout
      module System.Environment.InstallPaths.Parameters
    , module Data.Default.Class

    -- * Get Directories File Path
    , getBaseDir
    , getBinDir
    , getDataDir
    , getSysconfigDir
    , getLibDir
    , getLibexecDir

    -- * Get Data File Path
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
import qualified System.Environment as Environment (getEnv)
import System.IO (IO, FilePath)

import System.FilePath ((</>))

import Data.Default.Class
import System.Environment.Executable (splitExecutablePath)

import System.Environment.InstallPaths.Parameters


-- | Return \"bin\" directory.
--
-- Algorithm is as follows:
--
-- 1. Check if environment variable for bin directory is defined. If it is,
--    then return its value. See 'binDirEnvVar' for additional information.
--
-- 2. Check if environment variable base dir is defined. If it is,
--    then return its value with modified using 'baseDirToBinDir'. See
--    'baseDirEnvVar' and 'baseDirToBinDir' for additional information.
--
-- 3. Get file path of currently executed program and return directory where it
--    resides and apply 'executableDirToBinDir' to it. See
--    'executableDirToBinDir' for additional details.
getBinDir :: Parameters -> IO FilePath
getBinDir params = do
    maybeBinDir <- getEnv binDirVariableName
    case maybeBinDir of
        Just binDir -> return binDir
        Nothing     -> do
            maybeBaseDir <- getEnv baseDirVariableName
            case maybeBaseDir of
                Just baseDir -> return $ modifyBaseDir baseDir
                Nothing      ->
                    modifyExecutableDir . fst <$> splitExecutablePath
  where
    baseDirVariableName = baseDirEnvVar params
    binDirVariableName  = binDirEnvVar params
    modifyBaseDir       = baseDirToBinDir params
    modifyExecutableDir = executableDirToBinDir params

-- | Return base direcotory where application is installed.
--
-- Algorithm is as follows:
--
-- 1. Check if environment variable @data_files_basedir@ is defined. If it is,
--    then return its value.
--
-- 2. Call 'getBaseDir' and drop last directory element (\"bin\").
getBaseDir :: Parameters -> IO FilePath
getBaseDir params = do
    maybeBaseDir <- getEnv baseDirVariableName
    case maybeBaseDir of
        Just baseDir -> return baseDir
        Nothing      -> binDirToBaseDir <$> getBinDir params
  where
    baseDirVariableName = baseDirEnvVar params
    binDirToBaseDir     = executableDirToBaseDir params

-- | Utility function that is used for implementing a lot of other functions
-- in this module. It is not exported.
stdGetDir
    :: (Parameters -> String)
    -- ^ Getter for environment variable name that is check for its value.
    -> (Parameters -> Modify FilePath)
    -- ^ Getter for path modification function that takes base directory and
    -- returns path to one of its subdirectories.
    -> Parameters
    -> IO FilePath
    -- ^ Absolute file path.
stdGetDir getEnvVarName getModifyFilePath params = do
    maybeDir <- getEnv envVarName
    case maybeDir of
        Just dir -> return dir
        Nothing  -> modifyFilePath <$> getBaseDir params
  where
    envVarName     = getEnvVarName params
    modifyFilePath = getModifyFilePath params

getLibDir :: Parameters -> IO FilePath
getLibDir = stdGetDir libDirEnvVar baseDirToLibDir

getDataDir :: Parameters -> IO FilePath
getDataDir = stdGetDir libDirEnvVar baseDirToLibDir

getSysconfigDir :: Parameters -> IO FilePath
getSysconfigDir = stdGetDir sysconfigDirEnvVar baseDirToSysconfigDir

getLibexecDir :: Parameters -> IO FilePath
getLibexecDir = stdGetDir libexecDirEnvVar baseDirToLibexecDir

getDataFileName :: Parameters -> FilePath -> IO FilePath
getDataFileName params relativeFilePath = (</> relativeFilePath) <$> getDataDir params

-- | Utility function that returns 'Nothing' if environment variable is not
-- defined. Otherwise it behaves as 'Environment.getEnv' form
-- "System.Environment" module.
getEnv
    :: String
    -- ^ Environment variable name, if its empty 'Nothing' is returned.
    -> IO (Maybe String)
getEnv ""  = return Nothing
getEnv var = (Just <$> Environment.getEnv var)
    `catch` forget ioException (return Nothing)
  where
    ioException = Proxy :: Proxy IOException

    forget :: Proxy b -> a -> b -> a
    forget Proxy = const

-- $directoryLayout
--
-- Default directory layout:
--
-- > ${base_dir}
-- > |-- bin/${executable}
-- > |-- etc/
-- > |-- lib/
-- > |-- libexec/
-- > `-- share/
--
-- Value of @${base_dir}@ can be adjusted either by environment variable
-- defined in 'baseDirEnvVar' or by changing function 'executableDirToBinDir'
-- that derives its value from directory path where currently running
-- executable was found.
--
-- Other directories have similar ways for overriding their value and deriving
-- it from @${base_dir}@. See 'Parameters' data type for details.
