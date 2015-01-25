{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- TODO
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

data Directory
    = BaseDir
    | BinDir
    | DataDir
    | LibDir
    | LibexecDir
    | SysconfigDir
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Show, Typeable)

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

-- | @'noEnvironmentVariable' = 'const' \"\"@
noEnvironmentVariable :: Directory -> String
noEnvironmentVariable = const ""

defaultBaseDirTo :: Directory -> Modify FilePath
defaultBaseDirTo dir = case dir of
    BaseDir      -> id
    BinDir       -> (</> "bin")
    DataDir      -> (</> "share")
    LibDir       -> (</> "lib")
    LibexecDir   -> (</> "libexec")
    SysconfigDir -> (</> "etc")

defaultExecutableDirTo :: Directory -> Modify FilePath
defaultExecutableDirTo dir = case dir of
    BaseDir      -> dropTrailingPathSeparator . dropFileName
    BinDir       -> dropTrailingPathSeparator
    _            -> defaultBaseDirTo dir . defaultExecutableDirTo BaseDir

-- }}} Symbolic Names For Directories -----------------------------------------

-- {{{ Parameters -------------------------------------------------------------

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
