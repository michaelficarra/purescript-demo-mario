module Mario.DOM where

import Control.Monad.Eff (Eff(..))

import DOM (DOM(..), Node(..))

type Coordinate = { x :: Number, y :: Number }

foreign import updatePosition """
  function updatePosition(node) {
    return function(coord) {
      return function() {
        node.style.left = coord.x + 'px';
        node.style.bottom = coord.y + 'px';
      };
    };
  }
  """ :: Node -> Coordinate -> forall eff. Eff (dom :: DOM | eff) Unit

foreign import updateSprite """
  function updateSprite(node) {
    var a = document.createElement('a');
    return function(url) {
      return function() {
        a.href = url;
        if (node.src !== a.href) node.src = url;
      };
    };
  }
  """ :: Node -> String -> forall eff. Eff (dom :: DOM | eff) Unit

foreign import onDOMContentLoaded """
  function onDOMContentLoaded(action) {
    if (document.readyState === 'interactive') {
      action();
    } else {
      document.addEventListener('DOMContentLoaded', action);
    }
    return function() { return {}; };
  }
  """ :: forall eff a. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMario
  "function getMario() { return document.getElementById('mario'); }"
  :: forall eff. Eff (dom :: DOM | eff) Node

