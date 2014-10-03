# Module Documentation

## Module Mario

### Types

    data Direction where
      Left :: Direction
      Right :: Direction

    data Verb where
      Jumping :: Verb
      Walking :: Verb
      Standing :: Verb


### Values

    accel :: Mario.GameState -> Number

    airAccel :: Number

    airFriction :: Number

    applyFriction :: Mario.GameState -> Mario.GameState

    applyGravity :: Mario.GameState -> Mario.GameState

    currentActivity :: Mario.GameState -> Mario.Verb

    friction :: Mario.GameState -> Number

    gravity :: Number

    groundAccel :: Number

    groundFriction :: Number

    groundHeight :: Number

    initialState :: Mario.GameState

    isAirborne :: Mario.GameState -> Boolean

    jump :: Boolean -> Mario.GameState -> Mario.GameState

    jumpKeyCode :: Number

    jumpSpeed :: Number

    leftKeyCode :: Number

    main :: forall t1838. Control.Monad.Eff.Eff t1838 Prelude.Unit

    marioLogic :: Mario.Inputs -> Mario.GameState -> Mario.GameState

    marioSpriteUrl :: Mario.Verb -> Mario.Direction -> String

    maxMoveSpeed :: Number

    mkInputs :: Boolean -> Boolean -> Boolean -> Mario.Inputs

    offsetGround :: Number -> Mario.DOM.Coordinate -> Mario.DOM.Coordinate

    rightKeyCode :: Number

    velocity :: Mario.GameState -> Mario.GameState

    walk :: Boolean -> Boolean -> Mario.GameState -> Mario.GameState



