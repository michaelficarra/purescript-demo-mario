module Mario where

import Prelude ((<<<), (*), (+), (-), (==), (<), (>), (<=), (<>), (&&), not)
import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Math (abs, max, min)

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
accel = 0.06 -- px / frame^2
maxMoveSpeed = 2.5 -- px / frame
friction = 0.1 -- px / frame^2
jumpSpeed = 6 -- px / frame


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
  currentActivity c | c.dx == 0 = Standing
  currentActivity _ = Walking

isAirborne :: Character -> Boolean
isAirborne c = c.y > 0

-- when Mario is in motion, his position changes
velocity :: Character -> Character
velocity c = c { x = c.x + c.dx, y = c.y + c.dy }

-- when Mario is above the ground, he is continuously pulled downward
applyGravity :: Character -> Character
applyGravity c | c.y <= -c.dy = c { y = 0, dy = 0 }
applyGravity c = c { dy = c.dy - gravity }

-- Mario can move himself left/right with a fixed acceleration
walk :: Boolean -> Boolean -> Character -> Character
walk true false c = c { dx = max (-maxMoveSpeed) (c.dx - accel), dir = Left }
walk false true c = c { dx = min maxMoveSpeed (c.dx + accel), dir = Right }
walk _ _ c = applyFriction c
  where
  -- Mario slows down when he is not attempting to move himself
  applyFriction :: Character -> Character
  applyFriction c | c.dx == 0 = c
  applyFriction c | abs c.dx <= friction = c { dx = 0 }
  applyFriction c | c.dx > 0 = c { dx = c.dx - friction }
  applyFriction c | c.dx < 0 = c { dx = c.dx + friction }

-- Mario can change his vertical acceleration when he is on the ground
jump :: Boolean -> Character -> Character
jump true c | not (isAirborne c) = c { dy = jumpSpeed }
jump false c | isAirborne c && c.dy > 0 = c { dy = c.dy - gravity }
jump _ c = c

marioLogic :: { left :: Boolean, right :: Boolean, jump :: Boolean } -> Character -> Character
marioLogic inputs = velocity <<< applyGravity <<< walk inputs.left inputs.right <<< jump inputs.jump
