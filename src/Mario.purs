module Mario where

import Prelude ((<<<), (*), (+), (-), (==), (<), (>), (<=), (<>), (&&), not)
import Control.Monad.Eff (Eff())
import DOM.Node.Types (Node())
import DOM (DOM())
import Math (abs, max, min)
import Data.Ring (negate)
import Partial.Unsafe (unsafePartial)

type Character = {
  node :: Node,
  x :: Number,
  y :: Number,
  dx :: Number,
  dy :: Number,
  dir :: Direction
}
data Direction = Left | Right
type SpriteDescriptor = String
data Activity = Walking | Standing | Jumping

gravity = 0.15 -- px / frame^2
maxMoveSpeed = 2.5 -- px / frame
groundAccel = 0.06 -- px / frame^2
airAccel = 0.04 -- px / frame^2
groundFriction = 0.1 -- px / frame^2
airFriction = 0.02 -- px / frame^2
jumpCoefficient = 0.8 -- px / frame^3
minJumpSpeed = 4.0 -- px / frame

charSpriteDescriptor :: Character -> SpriteDescriptor
charSpriteDescriptor c = "character " <> activityDesc (currentActivity c) <> " " <> dirDesc c.dir
  where
  activityDesc :: Activity -> String
  activityDesc Walking = "walk"
  activityDesc Standing = "stand"
  activityDesc Jumping = "jump"

  dirDesc :: Direction -> String
  dirDesc Left = "left"
  dirDesc Right = "right"

  currentActivity :: Character -> Activity
  currentActivity c | isAirborne c = Jumping
  currentActivity c | c.dx == 0.0 = Standing
  currentActivity _ = Walking

isAirborne :: Character -> Boolean
isAirborne c = c.y > 0.0

accel :: Character -> Number
accel c = if isAirborne c then airAccel else groundAccel

friction :: Character -> Number
friction c = if isAirborne c then airFriction else groundFriction

-- when Mario is in motion, his position changes
velocity :: Character -> Character
velocity c = c { x = c.x + c.dx, y = c.y + c.dy }

-- when Mario is above the ground, he is continuously pulled downward
applyGravity :: Character -> Character
applyGravity c | c.y <= -c.dy = c { y = 0.0, dy = 0.0 }
applyGravity c = c { dy = c.dy - gravity }

-- Mario can move himself left/right with a fixed acceleration
walk :: Partial => Boolean -> Boolean -> Character -> Character
walk true false c = c { dx = max (-maxMoveSpeed) (c.dx - accel c), dir = Left }
walk false true c = c { dx = min maxMoveSpeed (c.dx + accel c), dir = Right }
walk _ _ c = applyFriction c
  where
  -- Mario slows down when he is not attempting to move himself
  applyFriction :: Character -> Character
  applyFriction c | c.dx == 0.0 = c
  applyFriction c | abs c.dx <= friction c = c { dx = 0.0 }
  applyFriction c | c.dx > 0.0 = c { dx = c.dx - friction c }
  applyFriction c | c.dx < 0.0 = c { dx = c.dx + friction c }

-- Mario can change his vertical acceleration when he is on the ground, proportional to his current speed
jump :: Boolean -> Character -> Character
jump true c | not (isAirborne c) = c { dy = minJumpSpeed + jumpCoefficient * abs c.dx }
jump false c | isAirborne c && c.dy > 0.0 = c { dy = c.dy - gravity }
jump _ c = c

marioLogic :: { left :: Boolean, right :: Boolean, jump :: Boolean } -> Character -> Character
marioLogic inputs = unsafePartial (velocity <<< applyGravity <<< walk inputs.left inputs.right <<< jump inputs.jump)
