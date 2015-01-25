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
    ( Parameters(..)
    , Modify
    , disableOverrideViaEnvVars
    )
  where

import Data.Function ((.))
import Data.List ((++))
import Data.String (String)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (FilePath)

import System.FilePath ((</>), dropFileName, dropTrailingPathSeparator)

import Data.Default.Class (Default(def))


-- | Modfier of @a@ is an endomorphism @a -> a@.
type Modify a = a -> a

data Parameters = Parameters
    { baseDirEnvVar          :: String
    , binDirEnvVar           :: String
    , dataDirEnvVar          :: String
    , libDirEnvVar           :: String
    , libexecDirEnvVar       :: String
    , sysconfigDirEnvVar     :: String
    , baseDirToBinDir        :: Modify FilePath
    , baseDirToDataDir       :: Modify FilePath
    , baseDirToLibDir        :: Modify FilePath
    , baseDirToLibexecDir    :: Modify FilePath
    , baseDirToSysconfigDir  :: Modify FilePath
    , executableDirToBaseDir :: Modify FilePath
    , executableDirToBinDir  :: Modify FilePath
    }
  deriving (Generic, Typeable)

-- | Environment variables are defined the same way as Cabal does. Definition
-- looks like:
--
-- @
-- 'def' = 'Parameters'
--     { 'baseDirEnvVar'          = \"data_files_basedir\"
--     , 'binDirEnvVar'           = \"data_files_bindir\"
--     , 'dataDirEnvVar'          = \"data_files_datadir\"
--     , 'libDirEnvVar'           = \"data_files_libdir\"
--     , 'libexecDirEnvVar'       = \"data_files_libexecdir\"
--     , 'sysconfigDirEnvVar'     = \"data_files_sysconfigdir\"
--     , 'baseDirToBinDir'        = ('</>' \"bin\")
--     , 'baseDirToDataDir'       = ('</>' \"share\")
--     , 'baseDirToLibDir'        = ('</>' \"lib")
--     , 'baseDirToLibexecDir'    = ('</>' \"libexec\")
--     , 'baseDirToSysconfigDir'  = ('</>' \"etc\")
--     , 'executableDirToBaseDir' = 'dropTrailingPathSeparator' . 'dropFileName'
--     , 'executableDirToBinDir'  = 'dropTrailingPathSeparator'
--     }
-- @
instance Default Parameters where
    def = Parameters
        { baseDirEnvVar          = mkEnvVar "base"
        , binDirEnvVar           = mkEnvVar "bin"
        , dataDirEnvVar          = mkEnvVar "data"
        , libDirEnvVar           = mkEnvVar "lib"
        , libexecDirEnvVar       = mkEnvVar "libexec"
        , sysconfigDirEnvVar     = mkEnvVar "sysconfig"
        , baseDirToBinDir        = (</> "bin")
        , baseDirToDataDir       = (</> "share")
        , baseDirToLibDir        = (</> "lib")
        , baseDirToLibexecDir    = (</> "libexec")
        , baseDirToSysconfigDir  = (</> "etc")
        , executableDirToBaseDir = dropTrailingPathSeparator . dropFileName
        , executableDirToBinDir  = dropTrailingPathSeparator
        }
      where
        mkEnvVar dirId = "data_files_" ++ dirId ++ "dir"

-- | Set all environment variables to \"\" and therefore disable the
-- possibility to override directory layout using environment variables.
disableOverrideViaEnvVars :: Modify Parameters
disableOverrideViaEnvVars params = params
    { baseDirEnvVar          = ""
    , binDirEnvVar           = ""
    , dataDirEnvVar          = ""
    , libDirEnvVar           = ""
    , libexecDirEnvVar       = ""
    , sysconfigDirEnvVar     = ""
    }
