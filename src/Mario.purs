module Mario where

import Prelude ((<<<), (*), (+), (-), (==), (<), (>), (<=), (<>), (&&), not)
import Control.Monad.Eff (Eff())
import DOM (DOM(), Node())
import Math (abs, max, min)

type Character = { node :: Node }

marioLogic :: Character -> Character
marioLogic x = x
