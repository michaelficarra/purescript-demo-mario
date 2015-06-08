module Mario.DOM where

import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Prelude ((<<<), (+), Unit())

import Mario (Character())

groundHeight = 40.0 --px

foreign import updatePositionP :: forall eff. Character -> Eff (dom :: DOM | eff) Unit

updatePosition :: Character -> Eff _ Unit
updatePosition = updatePositionP <<< offsetY groundHeight
  where
  offsetY :: Number -> Character -> Character
  offsetY amount c = c { y = c.y + amount }

foreign import onDOMContentLoaded :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMarioNode :: forall eff. Eff (dom :: DOM | eff) Node
