module Mario.Main (main) where

import Control.Monad.Eff (Eff())
import Prelude ((<$>), (<*>), bind, return, unit, Unit())
import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)
import Signal.Time (Time())

import Mario (charSpriteDescriptor, marioLogic, Character(), Direction(Left, Right), SpriteDescriptor())
import Mario.DOM (getMarioNode, onDOMContentLoaded, updatePosition, updateSprite)

type GameState = { mario :: Character }

leftKeyCode = 37
rightKeyCode = 39
jumpKeyCode = 38

initialState :: Eff _ GameState
initialState = do
  marioNode <- getMarioNode
  return {
    mario: {
      node: marioNode,
      x: 50.0,
      y: 150.0,
      dx: 0.0,
      dy: 0.0,
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
  frames <- animationFrame
  leftInputs <- keyPressed leftKeyCode
  rightInputs <- keyPressed rightKeyCode
  jumpInputs <- keyPressed jumpKeyCode
  let inputs = { left: _, right: _, jump: _ } <$> leftInputs <*> rightInputs <*> jumpInputs
  let game = foldp gameLogic initialState (sampleOn frames inputs)
  runSignal (render <$> game)
