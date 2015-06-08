module Mario where

import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random)
import DOM (DOM(), Node())
import Math (abs, max, min)

type Character = { node :: Node, x :: Number, y :: Number, dx :: Number, dy :: Number, dir :: Direction }
type SpriteDescriptor = String
data Direction = Left | Right
data Activity = Walking | Standing | Jumping

gravity = 0.1 -- px / frame^2
maxMoveSpeed = 2.5 -- px / frame
groundAccel = 0.06 -- px / frame^2
airAccel = 0.04 -- px / frame^2
groundFriction = 0.1 -- px / frame^2
airFriction = 0.02 -- px / frame^2
jumpCoefficient = 0.4 -- px / frame^3
minJumpSpeed = 2.5 -- px / frame


charSpriteDescriptor :: Character -> SpriteDescriptor
charSpriteDescriptor s = "character " ++ activityDesc (currentActivity s) ++ " " ++ dirDesc s.dir
  where
    activityDesc :: Activity -> String
    activityDesc Walking = "walk"
    activityDesc Standing = "stand"
    activityDesc Jumping = "jump"
    dirDesc :: Direction -> String
    dirDesc Left = "left"
    dirDesc Right = "right"

isAirborne :: Character -> Boolean
isAirborne s = s.y > 0

accel :: Character -> Number
accel s = if isAirborne s then airAccel else groundAccel

friction :: Character -> Number
friction s = if isAirborne s then airFriction else groundFriction

currentActivity :: Character -> Activity
currentActivity s | isAirborne s = Jumping
currentActivity s | s.dx == 0 = Standing
currentActivity _ = Walking

-- when Mario is in motion, his position changes
velocity :: Character -> Character
velocity s = s { x = s.x + s.dx, y = s.y + s.dy }

-- when Mario is above the ground, he is continuously pulled downward
applyGravity :: Character -> Character
applyGravity s | s.y <= -s.dy = s { y = 0, dy = 0 }
applyGravity s = s { y = s.y + s.dy, dy = s.dy - gravity }

-- Mario can move himself left/right with a fixed acceleration
walk :: Boolean -> Boolean -> Character -> Character
walk true false s = s { dx = max (-maxMoveSpeed) (s.dx - accel s), dir = Left }
walk false true s = s { dx = min (maxMoveSpeed) (s.dx + accel s), dir = Right }
walk _ _ s = applyFriction s
  where
  -- Mario slows down when he is not attempting to move himself
  applyFriction :: Character -> Character
  applyFriction s | s.dx == 0 = s
  applyFriction s | abs s.dx <= friction s = s { dx = 0 }
  applyFriction s | s.dx > 0 = s { dx = s.dx - friction s }
  applyFriction s | s.dx < 0 = s { dx = s.dx + friction s }

-- Mario can move change his vertical acceleration when he is on the ground, proportional to his current speed
jump :: Boolean -> Character -> Character
jump true s | not (isAirborne s) = s { dy = minJumpSpeed + jumpCoefficient * abs s.dx }
jump false s | isAirborne s && s.dy > 0 = s { dy = s.dy - gravity }
jump _ s = s

marioLogic :: forall r. { left :: Boolean, right :: Boolean, jump :: Boolean | r } -> Character -> Character
marioLogic inputs = velocity <<< applyGravity <<< walk inputs.left inputs.right <<< jump inputs.jump
