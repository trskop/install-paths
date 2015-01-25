{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Parameters of functions in System.Environment.InstallPaths
--               module.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Parameters of functions in "System.Environment.InstallPaths" module.
module System.Environment.InstallPaths.Parameters
    (
    -- * Symbolic Names For Directories
    --
    -- | Enum for relevant directories and some functions for its
    -- interpretation.
      Directory(..)
    , defaultEnvironmentVariable
    , defaultBaseDirTo
    , defaultExecutableDirTo
    , noEnvironmentVariable

    -- * Parameters
    --
    -- | Parameters for function sin "System.Environment.RelativeDataFiles"
    -- module.
    , Parameters(..)
    , disableOverrideViaEnvVars

    -- * Utilities
    , Modify
    )
  where

import Prelude (Bounded, Enum)

import Data.Data (Data)
import Data.Eq (Eq)
import Data.Function ((.), const, id)
import Data.List ((++))
import Data.Ord (Ord)
import Data.String (String)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (FilePath)
import Text.Show (Show)

import System.FilePath ((</>), dropFileName, dropTrailingPathSeparator)

import Data.Default.Class (Default(def))


-- {{{ Utilities --------------------------------------------------------------

-- | Modfier of @a@ is an endomorphism @a -> a@.
type Modify a = a -> a

-- }}} Utilities --------------------------------------------------------------

-- {{{ Symbolic Names For Directories -----------------------------------------

-- | Enum of directly supported directories.
data Directory
    = BaseDir
    | BinDir
    | DataDir
    | LibDir
    | LibexecDir
    | SysconfigDir
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Show, Typeable)

-- | Return default environment variable name for a specific directory. If that
-- variable is found, when trying to get file path of that specific directory,
-- its value is used instead of deriving it from executable path.
--
-- @
-- 'defaultEnvironmentVariable' dir = case dir of
--     'BaseDir'      -> \"data_files_basedir\"
--     'BinDir'       -> \"data_files_bindir\"
--     'DataDir'      -> \"data_files_datadir\"
--     'LibDir'       -> \"data_files_libdir\"
--     'LibexecDir'   -> \"data_files_libexecdir\"
--     'SysconfigDir' -> \"data_files_sysconfigdir\"
-- @
defaultEnvironmentVariable :: Directory -> String
defaultEnvironmentVariable dir = case dir of
    BaseDir      -> mkEnvVar "base"
    BinDir       -> mkEnvVar "bin"
    DataDir      -> mkEnvVar "data"
    LibDir       -> mkEnvVar "lib"
    LibexecDir   -> mkEnvVar "libexec"
    SysconfigDir -> mkEnvVar "sysconfig"
  where
    mkEnvVar dirId = "data_files_" ++ dirId ++ "dir"

-- | If this function is used as a value of 'environmentVariable', then
-- functions in "System.Environment.InstallPaths" don't check environment
-- variables.
--
-- @'noEnvironmentVariable' = 'const' \"\"@
noEnvironmentVariable :: Directory -> String
noEnvironmentVariable = const ""

-- | Take base directory path and return path for a specified 'Directory'. For
-- base directory this function behaves as identity.
--
-- @
-- 'defaultBaseDirTo' dir = case dir of
--     'BaseDir'      -> 'id'
--     'BinDir'       -> ('</>' \"bin\")
--     'DataDir'      -> ('</>' \"share\")
--     'LibDir'       -> ('</>' \"lib\")
--     'LibexecDir'   -> ('</>' \"libexec\")
--     'SysconfigDir' -> ('</>' \"etc\")
-- @
defaultBaseDirTo :: Directory -> Modify FilePath
defaultBaseDirTo dir = case dir of
    BaseDir      -> id
    BinDir       -> (</> "bin")
    DataDir      -> (</> "share")
    LibDir       -> (</> "lib")
    LibexecDir   -> (</> "libexec")
    SysconfigDir -> (</> "etc")

-- | Default function for fuguring out directory file path from executable
-- path.
--
-- Normally this function is used only for 'BaseDir' and 'BinDir', but it
-- defaults to using 'defaultBaseDirTo' for other values of 'Directory'.
--
-- @
-- 'defaultExecutableDirTo' dir = case dir of
--     'BaseDir' -> 'dropTrailingPathSeparator' . 'dropFileName'
--     'BinDir'  -> 'dropTrailingPathSeparator'
--     _       -> 'defaultBaseDirTo' dir . 'defaultExecutableDirTo' 'BaseDir'
-- @
defaultExecutableDirTo :: Directory -> Modify FilePath
defaultExecutableDirTo dir = case dir of
    BaseDir -> dropTrailingPathSeparator . dropFileName
    BinDir  -> dropTrailingPathSeparator
    _       -> defaultBaseDirTo dir . defaultExecutableDirTo BaseDir

-- }}} Symbolic Names For Directories -----------------------------------------

-- {{{ Parameters -------------------------------------------------------------

-- | Parameters of functions in "System.Environment.InstallPaths" module.
data Parameters = Parameters
    { environmentVariable :: Directory -> String
    , baseDirTo           :: Directory -> Modify FilePath
    , executableDirTo     :: Directory -> Modify FilePath
    }
  deriving (Generic, Typeable)

-- | Environment variables are defined the same way as Cabal does. Definition
-- looks like:
--
-- @
-- 'def' = 'Parameters'
--     { 'environmentVariable' = 'defaultEnvironmentVariable'
--     , 'baseDirTo'           = 'defaultBaseDirTo'
--     , 'executableDirTo'     = 'defaultExecutableDirTo'
--     }
-- @
instance Default Parameters where
    def = Parameters
        { environmentVariable = defaultEnvironmentVariable
        , baseDirTo           = defaultBaseDirTo
        , executableDirTo     = defaultExecutableDirTo
        }

-- | Set 'environmentVariable' to 'noEnvironmentVariable' which returns \"\" for
-- every value of 'Directory' and so disables the possibility to override
-- directory layout using environment variables.
disableOverrideViaEnvVars :: Modify Parameters
disableOverrideViaEnvVars params =
    params{environmentVariable = noEnvironmentVariable}

-- }}} Parameters -------------------------------------------------------------
