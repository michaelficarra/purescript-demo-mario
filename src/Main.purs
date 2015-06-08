module Main (main) where

import Prelude ((<$>), (<*>), (<<<), (+), return, unit, Unit())
import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Signal (foldp, runSignal, sampleOn, Signal())
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
      c.node.style.left = c.x + "px";
      c.node.style.bottom = c.y + "px";
    };
  }
  """ :: forall eff. Character -> Eff (dom :: DOM | eff) Unit

updatePosition :: Character -> Eff _ Unit
updatePosition = updatePositionP <<< offsetY groundHeight
  where
  offsetY :: Number -> Character -> Character
  offsetY amount c = c { y = c.y + amount }

foreign import updateSpriteP """
  function updateSpriteP(node) {
  return function(className) {
    return function() {
      node.className = className;
    };
  };}
  """ :: forall eff. Node -> SpriteDescriptor -> Eff (dom :: DOM | eff) Unit

updateSprite :: forall eff. Character -> Eff _ Unit
updateSprite c = updateSpriteP c.node (charSpriteDescriptor c)

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
    x: -50,
    y: 0,
    dx: 3,
    dy: 6,
    dir: Right
  }
}

gameLogic :: { left :: Boolean, right :: Boolean, jump :: Boolean } -> Eff _ GameState -> Eff _ GameState
gameLogic inputs gameState = do
  gs <- gameState
  return (gs { mario = marioLogic inputs gs.mario })

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  updatePosition gs.mario
  updateSprite gs.mario

main :: Eff _ Unit
main = onDOMContentLoaded do
  marioNode <- getMarioNode
  let initialEffState = return (initialState marioNode)
  frames <- animationFrame
  leftInputs <- keyPressed leftKeyCode
  rightInputs <- keyPressed rightKeyCode
  jumpInputs <- keyPressed jumpKeyCode
  let inputs = { left: _, right: _, jump: _ } <$> leftInputs <*> rightInputs <*> jumpInputs
  let game = foldp gameLogic initialEffState (sampleOn frames inputs)
  runSignal (render <$> game)
