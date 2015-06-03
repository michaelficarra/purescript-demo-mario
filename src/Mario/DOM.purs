module Mario.DOM where

import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Prelude ((<<<), (+), Unit())

import Mario (Character())

foreign import onDOMContentLoaded :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMarioNode :: forall eff. Eff (dom :: DOM | eff) Node
