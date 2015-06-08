module Mario where

import Prelude ((<<<), (*), (+), (-), (==), (<), (>), (<=), (<>), (&&), not)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Random (random, randomInt, randomRange)
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
data Enemy = Goomba Character

gravity = 0.15 -- px / frame^2
maxMoveSpeed = 2.5 -- px / frame
groundAccel = 0.06 -- px / frame^2
airAccel = 0.04 -- px / frame^2
groundFriction = 0.1 -- px / frame^2
airFriction = 0.02 -- px / frame^2
jumpCoefficient = 0.8 -- px / frame^3
minJumpSpeed = 4 -- px / frame


foreign import createEnemyNode """
  function createEnemyNode() {
    return document.createElement("div");
  }
  """ :: forall eff. Eff (dom :: DOM | eff) Node

makeRandomEnemy :: Eff _ Enemy
makeRandomEnemy = do
  node <- createEnemyNode
  x <- randomInt 0 200
  return $ Goomba { node: node, x: x, dx: 0, y: 500, dy: 0, dir: Right }

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

enemySpriteDescriptor :: Enemy -> SpriteDescriptor
enemySpriteDescriptor e = "enemy goomba " ++ charSpriteDescriptor (enemyCharacter e)

enemyCharacter :: Enemy -> Character
enemyCharacter (Goomba s) = s

isAirborne :: Character -> Boolean
isAirborne c = c.y > 0

accel :: Character -> Number
accel c = if isAirborne c then airAccel else groundAccel

friction :: Character -> Number
friction c = if isAirborne c then airFriction else groundFriction

-- when Mario is in motion, his position changes
velocity :: Character -> Character
velocity c = c { x = c.x + c.dx, y = c.y + c.dy }

-- when Mario is above the ground, he is continuously pulled downward
applyGravity :: Character -> Character
applyGravity c | c.y <= -c.dy = c { y = 0, dy = 0 }
applyGravity c = c { dy = c.dy - gravity }

-- Mario can move himself left/right with a fixed acceleration
walk :: Boolean -> Boolean -> Character -> Character
walk true false c = c { dx = max (-maxMoveSpeed) (c.dx - accel c), dir = Left }
walk false true c = c { dx = min maxMoveSpeed (c.dx + accel c), dir = Right }
walk _ _ c = applyFriction c
  where
  -- Mario slows down when he is not attempting to move himself
  applyFriction :: Character -> Character
  applyFriction c | c.dx == 0 = c
  applyFriction c | abs c.dx <= friction c = c { dx = 0 }
  applyFriction c | c.dx > 0 = c { dx = c.dx - friction c }
  applyFriction c | c.dx < 0 = c { dx = c.dx + friction c }

-- Mario can change his vertical acceleration when he is on the ground, proportional to his current speed
jump :: Boolean -> Character -> Character
jump true c | not (isAirborne c) = c { dy = minJumpSpeed + jumpCoefficient * abs c.dx }
jump false c | isAirborne c && c.dy > 0 = c { dy = c.dy - gravity }
jump _ c = c

marioLogic :: { left :: Boolean, right :: Boolean, jump :: Boolean } -> Character -> Character
marioLogic inputs = velocity <<< applyGravity <<< walk inputs.left inputs.right <<< jump inputs.jump

enemyLogic :: Enemy -> Enemy
enemyLogic (Goomba c) = Goomba $ (velocity <<< applyGravity) c
