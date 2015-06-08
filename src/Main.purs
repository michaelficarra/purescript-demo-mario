module Main (main) where

import Prelude ((<$>), (<*>), (<<<), (+), return, unit, Unit())
import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)

import Mario (marioLogic, Character())

type GameState = { mario :: Character }

groundHeight = 40 -- px


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
    x: 50,
    y: 150,
    dx: 0,
    dy: 0
  }
}

gameLogic :: Number -> Eff _ GameState -> Eff _ GameState
gameLogic frameCounter gameState = do
  gs <- gameState
  return (gs { mario = marioLogic gs.mario })

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  updatePosition gs.mario

main :: Eff _ Unit
main = onDOMContentLoaded do
  marioNode <- getMarioNode
  let initialEffState = return (initialState marioNode)
  frames <- animationFrame
  let game = foldp gameLogic initialEffState frames
  runSignal (render <$> game)
