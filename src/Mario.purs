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
  dy :: Number
}

gravity = 0.15 -- px / frame^2
accel = 0.06 -- px / frame^2
maxMoveSpeed = 2.5 -- px / frame
friction = 0.1 -- px / frame^2


-- when Mario is in motion, his position changes
velocity :: Character -> Character
velocity c = c { x = c.x + c.dx, y = c.y + c.dy }

-- when Mario is above the ground, he is continuously pulled downward
applyGravity :: Character -> Character
applyGravity c | c.y <= -c.dy = c { y = 0, dy = 0 }
applyGravity c = c { dy = c.dy - gravity }

-- Mario can move himself left/right with a fixed acceleration
walk :: Boolean -> Boolean -> Character -> Character
walk true false c = c { dx = max (-maxMoveSpeed) (c.dx - accel) }
walk false true c = c { dx = min maxMoveSpeed (c.dx + accel) }
walk _ _ c = applyFriction c
  where
  -- Mario slows down when he is not attempting to move himself
  applyFriction :: Character -> Character
  applyFriction c | c.dx == 0 = c
  applyFriction c | abs c.dx <= friction = c { dx = 0 }
  applyFriction c | c.dx > 0 = c { dx = c.dx - friction }
  applyFriction c | c.dx < 0 = c { dx = c.dx + friction }

marioLogic :: { left :: Boolean, right :: Boolean } -> Character -> Character
marioLogic inputs = velocity <<< applyGravity <<< walk inputs.left inputs.right
