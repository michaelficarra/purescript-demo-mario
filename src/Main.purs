module Main where

import Control.Monad.Eff (Eff(..))
import Data.Foldable (foldl)
import Data.Traversable (sequence)
import DOM (DOM(..), Node(..))
import Signal ((~>), constant, foldp, merge, runSignal, sampleOn, Signal(..))
import Signal.DOM (animationFrame, keyPressed)

import Mario (marioLogic, initialState, marioSpriteUrl, currentActivity)

type Coordinate = { x :: Number, y :: Number }


groundHeight = 40 -- px

-- NOTE: subtracts 4 pixels for mario sprite offset
offsetGround :: Number -> Coordinate -> Coordinate
offsetGround amount pos = pos { y = pos.y + amount - 4 }

foreign import updatePosition """
  function updatePosition(node) {
    return function(coord) {
      return function() {
        node.style.left = coord.x + "px";
        node.style.bottom = coord.y + "px";
      };
    };
  }
  """ :: Node -> Coordinate -> forall eff. Eff (dom :: DOM | eff) Unit

foreign import updateSprite """
  function updateSprite(node) {
    var a = document.createElement("a");
    return function(url) {
      return function() {
        a.href = url;
        if (node.src !== a.href) node.src = url;
      };
    };
  }
  """ :: Node -> String -> forall eff. Eff (dom :: DOM | eff) Unit


type Inputs = { right :: Boolean, left :: Boolean, jump :: Boolean }

-- left arrow, a
leftKeyCodes = [37, 65]
-- right arrow, d
rightKeyCodes = [39, 68]
-- up arrow, w
jumpKeyCodes = [38, 87]

combineKeyPresses :: [Signal Boolean] -> Signal Boolean
combineKeyPresses = foldl merge (constant false)

mkInputs :: Boolean -> Boolean -> Boolean -> Inputs
mkInputs l r j = { left: l, right: r, jump: j }


foreign import onDOMContentLoaded """
  function onDOMContentLoaded(action) {
    if (document.readyState === "interactive") {
      action();
    } else {
      document.addEventListener("DOMContentLoaded", action);
    }
    return function() { return {}; };
  }
  """ :: forall eff a. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMario """
  function getMario() { return document.getElementById("mario"); }
  """ :: forall eff. Eff (dom :: DOM | eff) Node


main = onDOMContentLoaded do
  marioElement <- getMario
  leftInputs <- combineKeyPresses <$> sequence (keyPressed <$> leftKeyCodes)
  rightInputs <- combineKeyPresses <$> sequence (keyPressed <$> rightKeyCodes)
  jumpInputs <- combineKeyPresses <$> sequence (keyPressed <$> jumpKeyCodes)
  let inputs = mkInputs <$> leftInputs <*> rightInputs <*> jumpInputs
  frames <- animationFrame
  runSignal $ foldp marioLogic initialState (sampleOn frames inputs) ~> \gameState -> do
    updateSprite marioElement $ marioSpriteUrl (currentActivity gameState) gameState.dir
    updatePosition marioElement (offsetGround groundHeight {x: gameState.x, y: gameState.y})
