module Quail.Server.Resolve where

import Control.Monad (guard)
import Control.Monad.Extra (maybeM)
import Data.Either.Extra (eitherToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful (Eff, (:>))
import Effectful.FileSystem (FileSystem, doesDirectoryExist, doesFileExist)
import Effectful.Haxl (Haxl, haxl)
import Effectful.NonDet
    ( OnEmptyPolicy (OnEmptyKeep)
    , emptyEff
    , runNonDet
    , (<|>)
    )
import Quail.Server.Files (inRootDir)
import Quail.Server.SQLite (UrlEntity)
import Quail.Server.SQLite qualified as SQLite
import Prelude

data Result
    = Directory FilePath
    | File FilePath
    | Entity UrlEntity

resolve :: (FileSystem :> es, Haxl :> es) => Text -> Eff es (Maybe Result)
resolve stub = fmap eitherToMaybe . runNonDet OnEmptyKeep $ tryDir <|> tryFile <|> tryEntity
  where
    filepath = inRootDir (Text.unpack stub)
    tryFile = do
        guard =<< doesFileExist filepath
        pure $ File filepath
    tryDir = do
        guard =<< doesDirectoryExist filepath
        pure $ Directory filepath
    tryEntity = maybeM emptyEff (pure . Entity) . haxl $ SQLite.getUrlByStub stub
