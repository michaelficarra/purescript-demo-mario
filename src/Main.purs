module Main (main) where

import Prelude ((<$>), (<*>), (<<<), (+), return, unit, Unit())
import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)

import Mario (marioLogic, Character())

type GameState = { mario :: Character }


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
initialState marioNode = { mario: { node: marioNode } }

gameLogic :: Number -> Eff _ GameState -> Eff _ GameState
gameLogic frameCounter gameState = do
  gs <- gameState
  return (gs { mario = marioLogic gs.mario })

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  return unit

main :: Eff _ Unit
main = onDOMContentLoaded do
  marioNode <- getMarioNode
  let initialEffState = return (initialState marioNode)
  frames <- animationFrame
  let game = foldp gameLogic initialEffState frames
  runSignal (render <$> game)
