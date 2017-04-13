module Mario.Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (TIMER)
import DOM (DOM)
import Signal (foldp, runSignal, sampleOn)
import Signal.DOM (animationFrame, keyPressed)

import Mario (marioLogic, Character, Direction(..))
import Mario.DOM (getMarioNode, onDOMContentLoaded, updatePosition, updateSprite)

type GameState = { mario :: Character }

leftKeyCode :: Int
leftKeyCode = 37

rightKeyCode :: Int
rightKeyCode = 39

jumpKeyCode :: Int
jumpKeyCode = 38

initialState :: forall eff. Eff (dom :: DOM | eff) GameState
initialState = do
  marioNode <- getMarioNode
  pure {
    mario: {
      node: marioNode,
      x: -50.0,
      y: 0.0,
      dx: 3.0,
      dy: 6.0,
      dir: Right
    }
  }

gameLogic :: forall eff. { left :: Boolean, right :: Boolean, jump :: Boolean } -> Eff (dom :: DOM | eff) GameState -> Eff (dom :: DOM | eff) GameState
gameLogic inputs gameState = do
  gs <- gameState
  pure (gs { mario = marioLogic inputs gs.mario })

render :: forall eff. Eff (dom :: DOM | eff) GameState -> Eff (dom :: DOM | eff) Unit
render gameState = do
  gs <- gameState
  updatePosition gs.mario
  updateSprite gs.mario

main :: forall eff. Eff (timer :: TIMER | eff) Unit
main = onDOMContentLoaded do
  frames <- animationFrame
  leftInputs <- keyPressed leftKeyCode
  rightInputs <- keyPressed rightKeyCode
  jumpInputs <- keyPressed jumpKeyCode
  let inputs = { left: _, right: _, jump: _ } <$> leftInputs <*> rightInputs <*> jumpInputs
  let game = foldp gameLogic initialState (sampleOn frames inputs)
  runSignal (render <$> game)
