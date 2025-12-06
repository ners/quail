module Quail.Server.Gql where

import Control.Exception (SomeException)
import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError (..))
import Data.Functor.Identity (Identity (runIdentity))
import Data.Hashable (Hashable)
import Data.Morpheus (deriveApp)
import Data.Morpheus.Subscriptions (Event (..))
import Data.Morpheus.Types
import Data.String (IsString (fromString))
import Data.Typeable (Typeable)
import Effectful (Eff, (:>))
import Effectful.Haxl (Haxl, haxl)
import GHC.Generics (Generic)
import Haxl.Core (GenHaxl)
import Quail.GqlSchema
import Quail.Server.SQLite qualified as SQLite
import Prelude

data Channel = NewUrl
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Hashable)

type Event' = Event Channel (Url Identity)

rootResolver :: forall es. (Haxl :> es) => RootResolver (Eff es) Event' Query Mutation Subscription
rootResolver =
    RootResolver
        { queryResolver =
            Query
                { getAllUrls = haxl' (fmap fromUrlEntity) $ Right <$> SQLite.getAllUrls
                , getUrlByStub = \(Arg stub) -> haxl' (fmap fromUrlEntity) $ Right <$> SQLite.getUrlByStub stub
                }
        , mutationResolver =
            Mutation
                { createUrl = \CreateUrlArgs{..} -> haxl' fromUrlEntity $ SQLite.createUrl stub target
                , deleteUrl = \(Arg stub) -> haxl' fromUrlEntity $ SQLite.deleteUrl stub
                }
        , subscriptionResolver =
            Subscription
                { newUrl = subscribe NewUrl $ pure \(Event _ Url{..}) -> pure Url{stub = pure $ runIdentity stub, target = pure $ runIdentity target}
                }
        }
  where
    fromUrlEntity :: (Applicative f) => SQLite.UrlEntity -> Url f
    fromUrlEntity SQLite.UrlEntity{..} = Url{stub = pure stub, target = pure target}

    haxl' :: (WithOperation o) => (a -> b) -> GenHaxl () () (Either SomeException a) -> Resolver o Event' (Eff es) b
    haxl' f = either (throwError . fromString . show) (pure . f) <=< lift . haxl

gqlApp :: (Typeable es, Haxl :> es) => App Event' (Eff es)
gqlApp = deriveApp rootResolver
