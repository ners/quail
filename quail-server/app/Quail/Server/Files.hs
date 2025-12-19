{-# LANGUAGE CPP #-}

module Quail.Server.Files where

import Control.Monad.Extra (ifM)
import Effectful
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Prelude

#ifndef ROOT_DIR
#define ROOT_DIR "."
#endif

inRootDir :: FilePath -> FilePath
inRootDir = (ROOT_DIR </>)

staticFile :: (FileSystem :> es) => FilePath -> Eff es (Maybe FilePath)
staticFile (inRootDir -> f) = ifM (doesFileExist f) (pure $ Just f) (pure Nothing)

staticDir :: (FileSystem :> es) => FilePath -> Eff es (Maybe FilePath)
staticDir (inRootDir -> d) = ifM (doesDirectoryExist d) (pure $ Just d) (pure Nothing)
