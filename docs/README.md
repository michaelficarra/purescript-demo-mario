## Module Mario

#### `Character`

``` purescript
type Character = { node :: Node, x :: Number, y :: Number, dx :: Number, dy :: Number, dir :: Direction }
```

#### `Direction`

``` purescript
data Direction
  = Left
  | Right
```

#### `SpriteDescriptor`

``` purescript
type SpriteDescriptor = String
```

#### `Activity`

``` purescript
data Activity
  = Walking
  | Standing
  | Jumping
```

#### `gravity`

``` purescript
gravity :: Number
```

#### `maxMoveSpeed`

``` purescript
maxMoveSpeed :: Number
```

#### `groundAccel`

``` purescript
groundAccel :: Number
```

#### `airAccel`

``` purescript
airAccel :: Number
```

#### `groundFriction`

``` purescript
groundFriction :: Number
```

#### `airFriction`

``` purescript
airFriction :: Number
```

#### `jumpCoefficient`

``` purescript
jumpCoefficient :: Number
```

#### `minJumpSpeed`

``` purescript
minJumpSpeed :: Number
```

#### `charSpriteDescriptor`

``` purescript
charSpriteDescriptor :: Character -> SpriteDescriptor
```

#### `isAirborne`

``` purescript
isAirborne :: Character -> Boolean
```

#### `accel`

``` purescript
accel :: Character -> Number
```

#### `friction`

``` purescript
friction :: Character -> Number
```

#### `velocity`

``` purescript
velocity :: Character -> Character
```

#### `applyGravity`

``` purescript
applyGravity :: Character -> Character
```

#### `walk`

``` purescript
walk :: Boolean -> Boolean -> Character -> Character
```

#### `jump`

``` purescript
jump :: Boolean -> Character -> Character
```

#### `marioLogic`

``` purescript
marioLogic :: { left :: Boolean, right :: Boolean, jump :: Boolean } -> Character -> Character
```



