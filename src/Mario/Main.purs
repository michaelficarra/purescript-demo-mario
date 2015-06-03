module Mario.Main (main) where

import Control.Monad.Eff (Eff())
import Prelude ((<$>), (<*>), bind, return, unit, Unit())
import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)
import Signal.Time (Time())

import Mario (marioLogic, Character())
import Mario.DOM (getMarioNode, onDOMContentLoaded)

type GameState = { mario :: Character }

initialState :: Eff _ GameState
initialState = do
  marioNode <- getMarioNode
  return {
    mario: {
      node: marioNode
    }
  }

gameLogic :: Time -> Eff _ GameState -> Eff _ GameState
gameLogic currentTime gameState = do
  gs <- gameState
  return (gs { mario = marioLogic gs.mario })

render :: Eff _ GameState -> Eff _ Unit
render gameState = do
  gs <- gameState
  return unit

main :: Eff _ Unit
main = onDOMContentLoaded do
  frames <- animationFrame
  let game = foldp gameLogic initialState frames
  runSignal (render <$> game)
