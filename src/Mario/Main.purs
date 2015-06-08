module Mario.Main (main) where

import Control.Monad.Eff (Eff())
import Prelude ((<$>), (<*>), bind, return, unit, Unit())
import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)
import Signal.Time (Time())

import Mario (marioLogic, Character())
import Mario.DOM (getMarioNode, onDOMContentLoaded, updatePosition)

type GameState = { mario :: Character }

initialState :: Eff _ GameState
initialState = do
  marioNode <- getMarioNode
  return {
    mario: {
      node: marioNode,
      x: 50.0,
      y: 150.0,
      dx: 0.0,
      dy: 0.0
    }
  }

gameLogic :: Time -> Eff _ GameState -> Eff _ GameState
gameLogic currentTime gameState = do
  gs <- gameState
  return (gs { mario = marioLogic gs.mario })

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  updatePosition gs.mario

main :: Eff _ Unit
main = onDOMContentLoaded do
  frames <- animationFrame
  let game = foldp gameLogic initialState frames
  runSignal (render <$> game)
