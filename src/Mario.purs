module Mario where

import Math (abs, max, min)

data Direction = Left | Right
data Activity = Jumping | Walking | Standing
type GameState = { x :: Number, y :: Number, dx :: Number, dy :: Number, dir :: Direction }

initialState :: GameState
initialState = jump true {
    x: -40, y: 0,
    dx: maxMoveSpeed, dy: 0,
    dir: Right
  }


gravity = 0.2 -- px / frame^2

jumpCoefficient = 0.4
minJumpHeight = 3 -- px / frame
maxMoveSpeed = 2.5 -- px / frame

groundAccel = 0.06 -- px / frame^2
airAccel = 0.04 -- px / frame^2

groundFriction = 0.1 -- px / frame^2
airFriction = 0.02 -- px / frame^2


marioSpriteUrl :: Activity -> Direction -> String
marioSpriteUrl activity dir = "resources/mario/" ++ activityPath activity ++ "/" ++ dirPath dir ++ ".gif"
  where
    activityPath :: Activity -> String
    activityPath Jumping = "jump"
    activityPath Walking = "walk"
    activityPath Standing = "stand"
    dirPath :: Direction -> String
    dirPath Left = "left"
    dirPath Right = "right"

isAirborne :: GameState -> Boolean
isAirborne s = s.y > 0

currentActivity :: GameState -> Activity
currentActivity s | isAirborne s = Jumping
currentActivity s | s.dx == 0 = Standing
currentActivity _ = Walking

accel :: GameState -> Number
accel s = if isAirborne s then airAccel else groundAccel

friction :: GameState -> Number
friction s = if isAirborne s then airFriction else groundFriction


velocity :: GameState -> GameState
velocity s = s { x = s.x + s.dx, y = s.y + s.dy }

applyGravity :: GameState -> GameState
applyGravity s | s.y <= -s.dy = s { y = 0, dy = 0 }
applyGravity s = s { y = s.y + s.dy, dy = s.dy - gravity }

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
walk _ _ s = applyFriction s

applyFriction :: GameState -> GameState
applyFriction s | s.dx == 0 = s
applyFriction s | abs s.dx <= friction s = s { dx = 0 }
applyFriction s | s.dx > 0 = s { dx = s.dx - friction s }
applyFriction s | s.dx < 0 = s { dx = s.dx + friction s }

marioLogic :: forall r. {left :: Boolean, right :: Boolean, jump :: Boolean | r} -> GameState -> GameState
marioLogic inputs =
  velocity
  <<< applyGravity
  <<< walk inputs.left inputs.right
  <<< jump inputs.jump
