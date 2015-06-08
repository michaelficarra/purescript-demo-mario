module Main where

import Control.Monad (foldM)
import Control.Monad.Eff (Eff())
import Data.Array (filter, length, take)
import Data.Foldable (foldr)
import Data.Traversable (for, sequence)
import DOM (DOM(), Node())
import Signal ((~>), constant, foldp, merge, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)

import Mario (marioLogic, charSpriteDescriptor, Character(), SpriteDescriptor(), Direction(Left, Right))

type GameState = { mario :: Character }

groundHeight = 40 -- px
leftKeyCode = 37
rightKeyCode = 39
jumpKeyCode = 38


foreign import updatePositionP """
  function updatePositionP(c) {
    return function() {
      if (c.node.parentNode == null) {
        document.body.insertBefore(c.node, document.body.firstChild);
      }
      c.node.style.left = c.x + "px";
      c.node.style.bottom = c.y + "px";
    };
  }
  """ :: forall eff. Character -> Eff (dom :: DOM | eff) Unit

updatePosition :: forall eff. Character -> Eff _ Unit
updatePosition = updatePositionP <<< offsetGround groundHeight
  where
  offsetGround :: Number -> Character -> Character
  offsetGround amount pos = pos { y = pos.y + amount }

foreign import updateSpriteP """
  function updateSpriteP(node) {
  return function(className) {
    return function() {
      node.className = className;
    };
  };}
  """ :: forall eff. Node -> SpriteDescriptor -> Eff (dom :: DOM | eff) Unit

updateSprite :: forall eff. Character -> Eff _ Unit
updateSprite c = updateSpriteP c.node $ charSpriteDescriptor c

foreign import onDOMContentLoaded """
  function onDOMContentLoaded(action) {
    if (document.readyState === "interactive") {
      action();
    } else {
      document.addEventListener("DOMContentLoaded", action);
    }
    return function() { return {}; };
  }
  """ :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (eff) Unit

foreign import getMarioNode """
  function getMarioNode() { return document.getElementById("mario"); }
  """ :: forall eff. Eff (dom :: DOM | eff) Node


initialState :: Node -> GameState
initialState marioNode = {
  mario: {
    node: marioNode,
    x: -50, y: 0,
    dx: 3, dy: 3,
    dir: Right
  }
}

gameLogic :: forall r. { left :: Boolean, right :: Boolean, jump :: Boolean | r } -> Eff _ GameState -> Eff _ GameState
gameLogic inputs gameState = do
  gs <- gameState
  return $ gs { mario = marioLogic inputs gs.mario }

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  updatePosition gs.mario
  updateSprite gs.mario

main :: Eff _ Unit
main = onDOMContentLoaded do
  marioNode <- getMarioNode
  frames <- animationFrame
  leftInputs <- keyPressed leftKeyCode
  rightInputs <- keyPressed rightKeyCode
  jumpInputs <- keyPressed jumpKeyCode
  let inputs = { left: _, right: _, jump: _ } <$> leftInputs <*> rightInputs <*> jumpInputs
  let game = foldp gameLogic (pure $ initialState marioNode) (sampleOn frames inputs) ~> render
  runSignal game
