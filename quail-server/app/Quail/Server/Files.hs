{-# LANGUAGE CPP #-}

module Quail.Server.Files where

import Control.Monad.Extra (ifM)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Effectful
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Prelude

#ifndef ROOT_DIR
#define ROOT_DIR "."
#endif

inRootDir :: FilePath -> FilePath
inRootDir f = ROOT_DIR </> fromMaybe f (List.stripPrefix "/" f)

staticFile :: (FileSystem :> es) => FilePath -> Eff es (Maybe FilePath)
staticFile (inRootDir -> f) = ifM (doesFileExist f) (pure $ Just f) (pure Nothing)

staticDir :: (FileSystem :> es) => FilePath -> Eff es (Maybe FilePath)
staticDir (inRootDir -> d) = ifM (doesDirectoryExist d) (pure $ Just d) (pure Nothing)
