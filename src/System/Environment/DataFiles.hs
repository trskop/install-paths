{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- TODO
module System.Environment.DataFiles
  where

import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Tuple (fst)
import System.IO (IO, FilePath)

import System.FilePath ((</>), dropTrailingPathSeparator)

import System.Environment.Executable (splitExecutablePath)


getBinDir :: IO FilePath
getBinDir = dropTrailingPathSeparator . fst <$> splitExecutablePath
--Environment variable: data_files_bindir

{-
getLibDir :: IO FilePath
Environment variable: data_files_libdir
-}
{-
getDataDir :: IO FilePath
Environment variable: data_files_datadir
-}
{-
configDir :: IO FilePath

Cabal calls it sysconfigDir.
Environment variable: data_files_sysconfdir
-}
{-
getLibexecdirDir :: IO FilePath
Environment variable: data_files_libexecdir
-}


{-
getDataFileName :: FilePath -> IO FilePath
-}
