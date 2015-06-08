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


-- when Mario is in motion, his position changes
velocity :: Character -> Character
velocity c = c { x = c.x + c.dx, y = c.y + c.dy }

-- when Mario is above the ground, he is continuously pulled downward
applyGravity :: Character -> Character
applyGravity c | c.y <= -c.dy = c { y = 0, dy = 0 }
applyGravity c = c { dy = c.dy - gravity }

marioLogic :: Character -> Character
marioLogic = velocity <<< applyGravity
