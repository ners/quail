{-# OPTIONS_GHC -Wno-unused-type-patterns #-}

module Effectful.Haxl where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Effectful
import Effectful.Dispatch.Static
import Haxl.Core (Env, GenHaxl, runHaxl)
import Prelude

data Haxl' u w :: Effect

type instance DispatchOf (Haxl' u w) = 'Static 'WithSideEffects

newtype instance StaticRep (Haxl' (u :: Type) (w :: Type)) = Haxl' (Env u w)

type Haxl = Haxl' () ()

runHaxl :: (IOE :> es) => Env u w -> Eff (Haxl' u w ': es) b -> Eff es b
runHaxl env = evalStaticRep (Haxl' env)

haxl
    :: forall u w a es
     . (Haxl' u w :> es, Monoid w)
    => GenHaxl u w a
    -> Eff es a
haxl g = do
    env <- getStaticRep @(Haxl' u w)
    unsafeEff_ $ Haxl.Core.runHaxl (coerce env) g
