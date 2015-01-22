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

import Control.Exception (IOException, catch)
import Control.Monad (Monad(return))
import Data.Function ((.), const)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import Data.Tuple (fst)
import System.Environment (getEnv)
import System.IO (IO, FilePath)

import System.FilePath ((</>), dropTrailingPathSeparator)

import System.Environment.Executable (splitExecutablePath)


getBinDir :: IO FilePath
getBinDir = do
    maybeDir <- getEnv' "data_files_bindir"
    case maybeDir of
        Just dir -> return dir
        Nothing -> dropTrailingPathSeparator . fst <$> splitExecutablePath

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

getEnv'
    :: String
    -> IO (Maybe String)
getEnv' var = (Just <$> getEnv var)
    `catch` forget ioException (return Nothing)
  where
    ioException = Proxy :: Proxy IOException

    forget :: Proxy b -> a -> b -> a
    forget Proxy = const
