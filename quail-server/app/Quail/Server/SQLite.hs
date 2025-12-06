{-# LANGUAGE QuasiQuotes #-}

module Quail.Server.SQLite where

import Control.Exception (Exception (..), SomeException (..))
import Control.Monad
import Data.Binary (Binary)
import Data.Hashable
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (FromRow, Only (..), ToRow)
import Database.SQLite.Simple qualified as SQLite.Simple
import Database.SQLite.Simple.QQ
import GHC.Generics (Generic)
import Haxl.Core
import Prelude

data UrlEntity = UrlEntity
    { stub :: Text
    , target :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromRow, ToRow)

type Haxl = GenHaxl () ()

getUrlByStub :: Text -> Haxl (Maybe UrlEntity)
getUrlByStub = uncachedRequest . GetUrlByStub

getAllUrls :: Haxl [UrlEntity]
getAllUrls = uncachedRequest GetAllUrls

createUrl :: Text -> Text -> Haxl (Either SomeException UrlEntity)
createUrl stub target = uncachedRequest $ CreateUrl stub target

updateUrl :: Text -> Text -> Haxl (Either SomeException UrlEntity)
updateUrl stub target = uncachedRequest $ UpdateUrl stub target

deleteUrl :: Text -> Haxl (Either SomeException UrlEntity)
deleteUrl = uncachedRequest . DeleteUrl

data Req a where
    GetAllUrls :: Req [UrlEntity]
    GetUrlByStub :: Text -> Req (Maybe UrlEntity)
    CreateUrl :: Text -> Text -> Req (Either SomeException UrlEntity)
    UpdateUrl :: Text -> Text -> Req (Either SomeException UrlEntity)
    DeleteUrl :: Text -> Req (Either SomeException UrlEntity)

deriving stock instance Eq (Req a)

deriving stock instance Show (Req a)

instance ShowP Req where showp = show

instance Hashable (Req a) where
    hashWithSalt s GetAllUrls = hashWithSalt s (0 :: Int)
    hashWithSalt s (GetUrlByStub stub) = hashWithSalt s (1 :: Int, stub)
    hashWithSalt s (CreateUrl stub target) = hashWithSalt s (2 :: Int, stub, target)
    hashWithSalt s (UpdateUrl stub target) = hashWithSalt s (3 :: Int, stub, target)
    hashWithSalt s (DeleteUrl stub) = hashWithSalt s (4 :: Int, stub)

instance StateKey Req where
    data State Req = UrlState {}

instance DataSourceName Req where
    dataSourceName _ = "UrlDataSource"

instance DataSource u Req where
    fetch _state _flags _urlEnv = SyncFetch $ mapM_ \(BlockedFetch req r) -> putSuccess r =<< run req

newtype AlreadyExists = AlreadyExists Text
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Binary)

instance Exception AlreadyExists where
    fromException = logicErrorFromException
    toException = logicErrorToException

headOr :: (Exception e) => e -> [a] -> Either SomeException a
headOr e = maybe (Left $ SomeException e) Right . listToMaybe

run :: Req a -> IO a
run GetAllUrls = query' [sql| SELECT * FROM urls |]
run (GetUrlByStub stub) = listToMaybe <$> query [sql| SELECT * FROM urls WHERE stub = ? |] (Only stub)
run (CreateUrl stub target) = headOr (AlreadyExists stub) <$> query [sql| INSERT OR IGNORE INTO urls VALUES (?, ?) RETURNING * |] (stub, target)
run (UpdateUrl stub target) = headOr (NotFound stub) <$> query [sql| UPDATE urls SET target = ? WHERE stub = ? RETURNING * |] (target, stub)
run (DeleteUrl stub) = headOr (NotFound stub) <$> query [sql| DELETE FROM urls WHERE stub = ? RETURNING * |] (Only stub)

query :: (SQLite.Simple.ToRow p, SQLite.Simple.FromRow r) => SQLite.Simple.Query -> p -> IO [r]
query q p = SQLite.Simple.withConnection "test.db" \c -> SQLite.Simple.query c q p

query' :: (SQLite.Simple.FromRow r) => SQLite.Simple.Query -> IO [r]
query' = flip query ()

execute_ :: (SQLite.Simple.ToRow p) => SQLite.Simple.Query -> p -> IO ()
execute_ q p = SQLite.Simple.withConnection "test.db" \c -> SQLite.Simple.execute c q p

-- | Runs the query and returns the number of affected rows.
execute :: (SQLite.Simple.ToRow p) => SQLite.Simple.Query -> p -> IO Int
execute q p = SQLite.Simple.withConnection "test.db" \c -> do
    SQLite.Simple.execute c q p
    SQLite.Simple.changes c

execute' :: SQLite.Simple.Query -> IO Int
execute' = flip execute ()

createUrlDb :: IO ()
createUrlDb =
    void . execute' $
        [sql|
            CREATE TABLE IF NOT EXISTS urls
                ( stub TEXT NOT NULL
                , target TEXT NOT NULL
                , PRIMARY KEY (stub)
                )
        |]

initUrlEnv :: IO (Env () ())
initUrlEnv = do
    let urlStore = stateSet UrlState{} stateEmpty
    initEnv urlStore ()
