module Mario where

import Data.Traversable (sequence)
import Data.Foldable (foldl)
import Math (abs, max, min)
import Signal ((~>), constant, foldp, merge, runSignal, sampleOn, Signal(..))
import Signal.DOM (animationFrame, keyPressed)

import Mario.DOM


initialState :: GameState
initialState = jump true {
    x: -40, y: 0,
    dx: maxMoveSpeed, dy: 0,
    dir: Right
  }

data Direction = Left | Right
instance showDirection :: Show Direction where
  show Left = "left"
  show Right = "right"

data Verb = Jumping | Walking | Standing
instance showVerb :: Show Verb where
  show Jumping = "jump"
  show Walking = "walk"
  show Standing = "stand"

type GameState = { x :: Number, y :: Number, dx :: Number, dy :: Number, dir :: Direction }
type Inputs = { right :: Boolean, left :: Boolean, jump :: Boolean }


leftKeyCodes = [37, 65] -- left arrow, a
rightKeyCodes = [39, 68] -- right arrow, d
jumpKeyCodes = [38, 87] -- up arrow, w

groundHeight = 40 -- px

gravity = 0.2 -- px / frame^2

jumpCoefficient = 0.4
minJumpHeight = 3 -- px / frame
maxMoveSpeed = 2.5 -- px / frame

groundAccel = 0.06 -- px / frame^2
airAccel = 0.04 -- px / frame^2

groundFriction = 0.1 -- px / frame^2
airFriction = 0.02 -- px / frame^2


marioSpriteUrl :: Verb -> Direction -> String
marioSpriteUrl verb dir = "resources/mario/" ++ show verb ++ "/" ++ show dir ++ ".gif"

offsetGround :: Number -> Coordinate -> Coordinate
offsetGround amount pos = pos { y = pos.y + amount - 4 } -- 4 pixels for image offset

combineKeyPresses :: [Signal Boolean] -> Signal Boolean
combineKeyPresses = foldl merge (constant false)

mkInputs :: Boolean -> Boolean -> Boolean -> Inputs
mkInputs l r j = { left: l, right: r, jump: j }

isAirborne :: GameState -> Boolean
isAirborne s = s.y > 0

currentActivity :: GameState -> Verb
currentActivity s | isAirborne s = Jumping
currentActivity s | s.dx /= 0 = Walking
currentActivity _ = Standing

accel :: GameState -> Number
accel s = if isAirborne s then airAccel else groundAccel

friction :: GameState -> Number
friction s = if isAirborne s then airFriction else groundFriction


velocity :: GameState -> GameState
velocity s = s { x = s.x + s.dx, y = s.y + s.dy }

applyGravity :: GameState -> GameState
applyGravity s =
  if s.y <= -s.dy
  then s { y = 0, dy = 0 }
  else s { y = s.y + s.dy, dy = s.dy - gravity }

jumpSpeed :: Number -> Number
jumpSpeed dx = minJumpHeight + jumpCoefficient * abs dx

jump :: Boolean -> GameState -> GameState
jump true s | not (isAirborne s) = s { dy = jumpSpeed s.dx }
jump false s | isAirborne s && s.dy > 0 = s { dy = s.dy - gravity }
jump _ s = s

walk :: Boolean -> Boolean -> GameState -> GameState
walk true false s =
  let s' = if s.dx > 0 then applyFriction s else s in
  s' { dx = max (-maxMoveSpeed) (s'.dx - accel s'), dir = Left }
walk false true s =
  let s' = if s.dx < 0 then applyFriction s else s in
  s' { dx = min maxMoveSpeed (s'.dx + accel s'), dir = Right }
walk x y s = applyFriction s

applyFriction :: GameState -> GameState
applyFriction s | s.dx == 0 = s
applyFriction s | abs s.dx <= friction s = s { dx = 0 }
applyFriction s | s.dx > 0 = s { dx = s.dx - friction s }
applyFriction s | s.dx < 0 = s { dx = s.dx + friction s }

marioLogic :: Inputs -> GameState -> GameState
marioLogic inputs = velocity <<< applyGravity
  <<< jump inputs.jump
  <<< walk inputs.left inputs.right


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
