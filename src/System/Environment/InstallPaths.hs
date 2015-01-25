{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Get installation paths based on executable path.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Get installation paths based on executable path.
module System.Environment.InstallPaths
    (
    -- * Environment Variables and Directory Layout
    -- $directoryLayout
      module System.Environment.InstallPaths.Parameters
    , module Data.Default.Class

    -- * Naming Conventions
    --
    -- | Functions ending with prime (e.g. 'getDir'') takes takes 'Directory'
    -- argument first and then 'Parameters'. It's the other way around for not
    -- primed functions.
    --
    -- Functions that end with underscore (@_@) don't take 'Parameters'
    -- argument at all, instead they use @'def' :: 'Parameters'@ value.

    -- * Get Directories File Path
    , getDir
    , getDir'
    , getDir_

    -- ** Get Specific Directory File Path
    --
    -- | Most of the time only one or few of installation directories are
    -- needed. In such cases these functions provide easier access. Plus they
    -- are named the same way as those in Cabal's @Path_\*@ module.
    , getBaseDir
    , getBaseDir_
    , getBinDir
    , getBinDir_
    , getDataDir
    , getDataDir_
    , getLibDir
    , getLibDir_
    , getLibexecDir
    , getLibexecDir_
    , getSysconfigDir
    , getSysconfigDir_

    -- * Get File Path
    , getFileName
    , getFileName'
    , getFileName_
    , getDataFileName
    , getDataFileName_
    )
  where

import Control.Exception (IOException, catch)
import Control.Monad (Monad(return))
import Data.Function ((.), ($), const, flip)
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


-- {{{ Get Directories File Path ----------------------------------------------

-- | Get file path of specified directory based on provided 'Parameters'.
--
-- Same as 'getDir'', but with flipped arguments.
getDir :: Parameters -> Directory -> IO FilePath
getDir = flip getDir'

-- | Get file path of specified directory based on provided 'Parameters'.
--
-- Same as 'getDir', but with flipped arguments.
getDir' :: Directory -> Parameters -> IO FilePath
getDir' dir = case dir of
    BaseDir      -> getBaseDir
    BinDir       -> getBinDir
    _            -> stdGetDir dir

-- | @'getDir_' = 'getDir' 'def'@
getDir_ :: Directory -> IO FilePath
getDir_ = getDir def

-- {{{ Get Specific Directories File Path -------------------------------------

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
    baseDirVariableName = environmentVariable params BaseDir
    binDirVariableName  = environmentVariable params BinDir
    modifyBaseDir       = baseDirTo params BinDir
    modifyExecutableDir = executableDirTo params BinDir

-- | @'getBinDir_' = 'getBinDir' 'def'@
getBinDir_ :: IO FilePath
getBinDir_ = getBinDir def

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
    baseDirVariableName = environmentVariable params BaseDir
    binDirToBaseDir     = executableDirTo params BaseDir

-- | @'getBaseDir_' = 'getBaseDir' 'def'@
getBaseDir_ :: IO FilePath
getBaseDir_ = getBaseDir def

getLibDir :: Parameters -> IO FilePath
getLibDir = stdGetDir LibDir

-- | @'getLibDir_' = 'getLibDir' 'def'@
getLibDir_ :: IO FilePath
getLibDir_ = getLibDir def

getDataDir :: Parameters -> IO FilePath
getDataDir = stdGetDir DataDir

-- | @'getBaseDir_' = 'getBaseDir' 'def'@
getDataDir_ :: IO FilePath
getDataDir_ = getDataDir def

getSysconfigDir :: Parameters -> IO FilePath
getSysconfigDir = stdGetDir SysconfigDir

-- | @'getBaseDir_' = 'getBaseDir' 'def'@
getSysconfigDir_ :: IO FilePath
getSysconfigDir_ = getSysconfigDir def

getLibexecDir :: Parameters -> IO FilePath
getLibexecDir = stdGetDir LibexecDir

-- | @'getLibexecDir_' = 'getLibexecDir' 'def'@
getLibexecDir_ :: IO FilePath
getLibexecDir_ = getLibexecDir def

-- }}} Get Specific Directories File Path -------------------------------------
-- }}} Get Directories File Path ----------------------------------------------

-- {{{ Get File Path ----------------------------------------------------------

getFileName :: Parameters -> Directory -> FilePath -> IO FilePath
getFileName = flip getFileName'

getFileName' :: Directory -> Parameters -> FilePath -> IO FilePath
getFileName' dir params relativeFilePath =
    (</> relativeFilePath) <$> getDir' dir params

-- | @'getFileName_' = 'getFileName' 'def'@
getFileName_ :: Directory -> FilePath -> IO FilePath
getFileName_ = getFileName def

getDataFileName :: Parameters -> FilePath -> IO FilePath
getDataFileName = getFileName' DataDir

-- | @'getDataFileName_' = 'getDataFileName' 'def'@
getDataFileName_ :: FilePath -> IO FilePath
getDataFileName_ = getDataFileName def

-- }}} Get File Path ----------------------------------------------------------

-- {{{ Utility functions (not exported) ---------------------------------------

-- | Utility function that is used for implementing a lot of other functions
-- in this module. It is not exported.
stdGetDir
    :: Directory
    -> Parameters
    -> IO FilePath
    -- ^ Absolute file path.
stdGetDir dir params = do
    maybeDir <- getEnv envVarName
    case maybeDir of
        Just d  -> return d
        Nothing -> modifyFilePath <$> getBaseDir params
  where
    envVarName     = environmentVariable params dir
    modifyFilePath = baseDirTo params dir

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

-- }}} Utility functions (not exported) ---------------------------------------

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
