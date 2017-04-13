module Mario.DOM where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.Types (Node)
import Prelude ((<<<), (+), Unit)

import Mario (charSpriteDescriptor, Character(), SpriteDescriptor())

groundHeight :: Number
groundHeight = 40.0 --px

foreign import updatePositionP :: forall eff. Character -> Eff (dom :: DOM | eff) Unit

updatePosition :: forall eff. Character -> Eff (dom :: DOM | eff) Unit
updatePosition = updatePositionP <<< offsetY groundHeight
  where
  offsetY :: Number -> Character -> Character
  offsetY amount c = c { y = c.y + amount }

foreign import updateSpriteP :: forall eff. Node -> SpriteDescriptor -> Eff (dom :: DOM | eff) Unit

updateSprite :: forall eff. Character -> Eff (dom :: DOM | eff) Unit
updateSprite c = updateSpriteP c.node (charSpriteDescriptor c)

foreign import onDOMContentLoaded :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMarioNode :: forall eff. Eff (dom :: DOM | eff) Node
