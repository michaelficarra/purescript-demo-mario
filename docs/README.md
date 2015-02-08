# Module Documentation

## Module Prelude

### Types


    data Ordering where
      LT :: Ordering
      GT :: Ordering
      EQ :: Ordering


    newtype Unit where
      Unit :: {  } -> Unit


### Type Classes


    class (Prelude.Apply f) <= Applicative f where

      pure :: forall a. a -> f a


    class (Prelude.Functor f) <= Apply f where

      (<*>) :: forall a b. f (a -> b) -> f a -> f b


    class (Prelude.Apply m) <= Bind m where

      (>>=) :: forall a b. m a -> (a -> m b) -> m b


    class Bits b where

      (.&.) :: b -> b -> b

      (.|.) :: b -> b -> b

      (.^.) :: b -> b -> b

      shl :: b -> Number -> b

      shr :: b -> Number -> b

      zshr :: b -> Number -> b

      complement :: b -> b


    class BoolLike b where

      (&&) :: b -> b -> b

      (||) :: b -> b -> b

      not :: b -> b


    class (Prelude.Semigroupoid a) <= Category a where

      id :: forall t. a t t


    class Eq a where

      (==) :: a -> a -> Boolean

      (/=) :: a -> a -> Boolean


    class Functor f where

      (<$>) :: forall a b. (a -> b) -> f a -> f b


    class (Prelude.Applicative m, Prelude.Bind m) <= Monad m where


    class Num a where

      (+) :: a -> a -> a

      (-) :: a -> a -> a

      (*) :: a -> a -> a

      (/) :: a -> a -> a

      (%) :: a -> a -> a

      negate :: a -> a


    class (Prelude.Eq a) <= Ord a where

      compare :: a -> a -> Prelude.Ordering


    class Semigroup a where

      (<>) :: a -> a -> a


    class Semigroupoid a where

      (<<<) :: forall b c d. a c d -> a b c -> a b d


    class Show a where

      show :: a -> String


### Values


    (#) :: forall a b. a -> (a -> b) -> b


    ($) :: forall a b. (a -> b) -> a -> b


    (++) :: forall s. (Prelude.Semigroup s) => s -> s -> s


    (:) :: forall a. a -> [a] -> [a]


    (<#>) :: forall f a b. (Prelude.Functor f) => f a -> (a -> b) -> f b


    (<) :: forall a. (Prelude.Ord a) => a -> a -> Boolean


    (<=) :: forall a. (Prelude.Ord a) => a -> a -> Boolean


    (>) :: forall a. (Prelude.Ord a) => a -> a -> Boolean


    (>=) :: forall a. (Prelude.Ord a) => a -> a -> Boolean


    (>>>) :: forall a b c d. (Prelude.Semigroupoid a) => a b c -> a c d -> a b d


    ap :: forall m a b. (Prelude.Monad m) => m (a -> b) -> m a -> m b


    asTypeOf :: forall a. a -> a -> a


    cons :: forall a. a -> [a] -> [a]


    const :: forall a b. a -> b -> a


    flip :: forall a b c. (a -> b -> c) -> b -> a -> c


    liftA1 :: forall f a b. (Prelude.Applicative f) => (a -> b) -> f a -> f b


    liftM1 :: forall m a b. (Prelude.Monad m) => (a -> b) -> m a -> m b


    otherwise :: Boolean


    refEq :: forall a. a -> a -> Boolean


    refIneq :: forall a. a -> a -> Boolean


    return :: forall m a. (Prelude.Monad m) => a -> m a


    unit :: Prelude.Unit


    void :: forall f a. (Prelude.Functor f) => f a -> f Prelude.Unit


## Module Prelude.Unsafe

### Values


    unsafeIndex :: forall a. [a] -> Number -> a


## Module Math

### Types


    type Radians = Number


### Values


    abs :: Number -> Number


    acos :: Number -> Math.Radians


    asin :: Number -> Math.Radians


    atan :: Number -> Math.Radians


    atan2 :: Number -> Number -> Math.Radians


    ceil :: Number -> Number


    cos :: Math.Radians -> Number


    e :: Number


    exp :: Number -> Number


    floor :: Number -> Number


    ln10 :: Number


    ln2 :: Number


    log :: Number -> Number


    log10e :: Number


    log2e :: Number


    max :: Number -> Number -> Number


    min :: Number -> Number -> Number


    pi :: Number


    pow :: Number -> Number -> Number


    round :: Number -> Number


    sin :: Math.Radians -> Number


    sqrt :: Number -> Number


    sqrt1_2 :: Number


    sqrt2 :: Number


    tan :: Math.Radians -> Number


## Module Mario

### Types


    data Direction where
      Left :: Direction
      Right :: Direction

      px / frame
      px / frame^2
      px / frame^2
      px / frame^2
      px / frame
      px / frame^2
      px / frame^2

    type GameState = { x :: Number, y :: Number, dx :: Number, dy :: Number, dir :: Mario.Direction }


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


    initialState :: Mario.GameState


    isAirborne :: Mario.GameState -> Boolean


    jump :: Boolean -> Mario.GameState -> Mario.GameState


    jumpCoefficient :: Number


    jumpSpeed :: Number -> Number


    marioLogic :: Boolean -> Boolean -> Boolean -> Mario.GameState -> Mario.GameState


    marioSpriteUrl :: Mario.Verb -> Mario.Direction -> String


    maxMoveSpeed :: Number


    minJumpHeight :: Number


    velocity :: Mario.GameState -> Mario.GameState


    walk :: Boolean -> Boolean -> Mario.GameState -> Mario.GameState


## Module Data.Function

### Types


    data Fn0 :: * -> *


    data Fn1 :: * -> * -> *


    data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *


    data Fn2 :: * -> * -> * -> *


    data Fn3 :: * -> * -> * -> * -> *


    data Fn4 :: * -> * -> * -> * -> * -> *


    data Fn5 :: * -> * -> * -> * -> * -> * -> *


    data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *


    data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *


    data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *


    data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *


### Values


    mkFn0 :: forall a. (Prelude.Unit -> a) -> Data.Function.Fn0 a


    mkFn1 :: forall a b. (a -> b) -> Data.Function.Fn1 a b


    mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Data.Function.Fn10 a b c d e f g h i j k


    mkFn2 :: forall a b c. (a -> b -> c) -> Data.Function.Fn2 a b c


    mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Data.Function.Fn3 a b c d


    mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Data.Function.Fn4 a b c d e


    mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Data.Function.Fn5 a b c d e f


    mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Data.Function.Fn6 a b c d e f g


    mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Data.Function.Fn7 a b c d e f g h


    mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Data.Function.Fn8 a b c d e f g h i


    mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Data.Function.Fn9 a b c d e f g h i j


    on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c


    runFn0 :: forall a. Data.Function.Fn0 a -> a


    runFn1 :: forall a b. Data.Function.Fn1 a b -> a -> b


    runFn10 :: forall a b c d e f g h i j k. Data.Function.Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k


    runFn2 :: forall a b c. Data.Function.Fn2 a b c -> a -> b -> c


    runFn3 :: forall a b c d. Data.Function.Fn3 a b c d -> a -> b -> c -> d


    runFn4 :: forall a b c d e. Data.Function.Fn4 a b c d e -> a -> b -> c -> d -> e


    runFn5 :: forall a b c d e f. Data.Function.Fn5 a b c d e f -> a -> b -> c -> d -> e -> f


    runFn6 :: forall a b c d e f g. Data.Function.Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g


    runFn7 :: forall a b c d e f g h. Data.Function.Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h


    runFn8 :: forall a b c d e f g h i. Data.Function.Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i


    runFn9 :: forall a b c d e f g h i j. Data.Function.Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j


## Module DOM

### Types


    data DOM :: !


    data Node :: *


    data NodeList :: *


## Module Control.Monad.Eff

### Types


    data Eff :: # ! -> * -> *


    type Pure (a :: *) = forall e. Control.Monad.Eff.Eff e a


### Values


    bindE :: forall e a b. Control.Monad.Eff.Eff e a -> (a -> Control.Monad.Eff.Eff e b) -> Control.Monad.Eff.Eff e b


    forE :: forall e. Number -> Number -> (Number -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit


    foreachE :: forall e a. [a] -> (a -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit


    returnE :: forall e a. a -> Control.Monad.Eff.Eff e a


    runPure :: forall a. Control.Monad.Eff.Pure a -> a


    untilE :: forall e. Control.Monad.Eff.Eff e Boolean -> Control.Monad.Eff.Eff e Prelude.Unit


    whileE :: forall e a. Control.Monad.Eff.Eff e Boolean -> Control.Monad.Eff.Eff e a -> Control.Monad.Eff.Eff e Prelude.Unit


## Module Control.Monad.Eff.Ref

### Types


    data Ref :: !


    data RefVal :: * -> *


### Values


    modifyRef :: forall s r. Control.Monad.Eff.Ref.RefVal s -> (s -> s) -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) Prelude.Unit


    modifyRef' :: forall s b r. Control.Monad.Eff.Ref.RefVal s -> (s -> { newState :: s, retVal :: b }) -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) b


    newRef :: forall s r. s -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) (Control.Monad.Eff.Ref.RefVal s)


    readRef :: forall s r. Control.Monad.Eff.Ref.RefVal s -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) s


    writeRef :: forall s r. Control.Monad.Eff.Ref.RefVal s -> s -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) Prelude.Unit


## Module Control.Monad.Eff.Ref.Unsafe

### Values


    unsafeRunRef :: forall eff a. Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | eff) a -> Control.Monad.Eff.Eff eff a


## Module Control.Monad.Eff.Unsafe

### Values


    unsafeInterleaveEff :: forall eff1 eff2 a. Control.Monad.Eff.Eff eff1 a -> Control.Monad.Eff.Eff eff2 a


## Module Control.Monad.ST

### Types


    data ST :: * -> !


    data STRef :: * -> * -> *


### Values


    modifySTRef :: forall a h r. Control.Monad.ST.STRef h a -> (a -> a) -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a


    newSTRef :: forall a h r. a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Control.Monad.ST.STRef h a)


    pureST :: forall a. (forall h r. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a) -> a


    readSTRef :: forall a h r. Control.Monad.ST.STRef h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a


    runST :: forall a r. (forall h. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a) -> Control.Monad.Eff.Eff r a


    writeSTRef :: forall a h r. Control.Monad.ST.STRef h a -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a


## Module Control.Timer

### Types


    type EffTimer (e :: # !) (a :: *) = Control.Monad.Eff.Eff (timer :: Control.Timer.Timer | e) a


    data Interval :: *


    type Milliseconds = Number


    data Timeout :: *


    data Timer :: !


### Values


    clearInterval :: forall e. Control.Timer.Interval -> Control.Timer.EffTimer e Prelude.Unit


    clearTimeout :: forall e. Control.Timer.Timeout -> Control.Timer.EffTimer e Prelude.Unit


    interval :: forall a e. Control.Timer.Milliseconds -> Control.Timer.EffTimer e a -> Control.Timer.EffTimer e Control.Timer.Interval


    timeout :: forall a e. Control.Timer.Milliseconds -> Control.Timer.EffTimer e a -> Control.Timer.EffTimer e Control.Timer.Timeout


## Module Debug.Trace

### Types


    data Trace :: !


### Values


    print :: forall a r. (Prelude.Show a) => a -> Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | r) Prelude.Unit


    trace :: forall r. String -> Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | r) Prelude.Unit


## Module Signal

### Types


    data Signal :: * -> *


### Values


    (<~) :: forall f a b. (Prelude.Functor f) => (a -> b) -> f a -> f b


    (~) :: forall f a b. (Prelude.Apply f) => f (a -> b) -> f a -> f b


    (~>) :: forall f a b. (Prelude.Functor f) => f a -> (a -> b) -> f b


    applySig :: forall a b. Signal.Signal (a -> b) -> Signal.Signal a -> Signal.Signal b


    constant :: forall a. a -> Signal.Signal a


    distinct :: forall a. (Prelude.Eq a) => Signal.Signal a -> Signal.Signal a


    distinct' :: forall a. Signal.Signal a -> Signal.Signal a


    foldp :: forall a b. (a -> b -> b) -> b -> Signal.Signal a -> Signal.Signal b


    keepIf :: forall a. (a -> Boolean) -> a -> Signal.Signal a -> Signal.Signal a


    map :: forall a b. (a -> b) -> Signal.Signal a -> Signal.Signal b


    merge :: forall a. Signal.Signal a -> Signal.Signal a -> Signal.Signal a


    runSignal :: forall e. Signal.Signal (Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit


    sampleOn :: forall a b. Signal.Signal a -> Signal.Signal b -> Signal.Signal b


    unwrap :: forall a e. Signal.Signal (Control.Monad.Eff.Eff e a) -> Control.Monad.Eff.Eff e (Signal.Signal a)


    zip :: forall a b c. (a -> b -> c) -> Signal.Signal a -> Signal.Signal b -> Signal.Signal c


## Module Signal.Channel

### Types


    data Chan :: !


    data Channel :: * -> *


### Values


    channel :: forall a e. a -> Control.Monad.Eff.Eff (chan :: Signal.Channel.Chan | e) (Signal.Channel.Channel a)


    send :: forall a e. Signal.Channel.Channel a -> a -> Control.Monad.Eff.Eff (chan :: Signal.Channel.Chan | e) Prelude.Unit


    subscribe :: forall a. Signal.Channel.Channel a -> Signal.Signal a


## Module Signal.Time

### Types

      |Returns the number of milliseconds since an arbitrary, but constant, time in the past.

    type Time = Number


### Values


    every :: Signal.Time.Time -> Signal.Signal Signal.Time.Time


    millisecond :: Signal.Time.Time


    now :: forall e. Control.Monad.Eff.Eff (timer :: Control.Timer.Timer | e) Signal.Time.Time


    second :: Signal.Time.Time


## Module Signal.DOM

### Types


    type CoordinatePair = { x :: Number, y :: Number }


    type Touch = { id :: String, screenX :: Number, screenY :: Number, clientX :: Number, clientY :: Number, pageX :: Number, pageY :: Number, radiusX :: Number, radiusY :: Number, rotationAngle :: Number, force :: Number }


### Values


    animationFrame :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM, timer :: Control.Timer.Timer | e) (Signal.Signal Signal.Time.Time)


    keyPressed :: forall e. Number -> Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Boolean)


    mouseButton :: forall e. Number -> Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Boolean)


    mousePos :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Signal.DOM.CoordinatePair)


    tap :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Boolean)


    touch :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal [Signal.DOM.Touch])


## Module Control.Monad

### Values


    foldM :: forall m a b. (Prelude.Monad m) => (a -> b -> m a) -> a -> [b] -> m a


    replicateM :: forall m a. (Prelude.Monad m) => Number -> m a -> m [a]


    unless :: forall m. (Prelude.Monad m) => Boolean -> m Prelude.Unit -> m Prelude.Unit


    when :: forall m. (Prelude.Monad m) => Boolean -> m Prelude.Unit -> m Prelude.Unit


## Module Control.Lazy

### Type Classes


    class Lazy l where

      defer :: (Prelude.Unit -> l) -> l


    class Lazy1 l where

      defer1 :: forall a. (Prelude.Unit -> l a) -> l a


    class Lazy2 l where

      defer2 :: forall a b. (Prelude.Unit -> l a b) -> l a b


### Values


    fix :: forall l a. (Control.Lazy.Lazy l) => (l -> l) -> l


    fix1 :: forall l a. (Control.Lazy.Lazy1 l) => (l a -> l a) -> l a


    fix2 :: forall l a b. (Control.Lazy.Lazy2 l) => (l a b -> l a b) -> l a b


## Module Control.Extend

### Type Classes


    class (Prelude.Functor w) <= Extend w where

      (<<=) :: forall b a. (w a -> b) -> w a -> w b


### Values


    (=<=) :: forall b a w c. (Control.Extend.Extend w) => (w b -> c) -> (w a -> b) -> w a -> c


    (=>=) :: forall b a w c. (Control.Extend.Extend w) => (w a -> b) -> (w b -> c) -> w a -> c


    (=>>) :: forall b a w. (Control.Extend.Extend w) => w a -> (w a -> b) -> w b


    duplicate :: forall a w. (Control.Extend.Extend w) => w a -> w (w a)


## Module Control.Comonad

### Type Classes


    class (Control.Extend.Extend w) <= Comonad w where

      extract :: forall a. w a -> a


## Module Control.Bind

### Values


    (<=<) :: forall a b c m. (Prelude.Bind m) => (b -> m c) -> (a -> m b) -> a -> m c


    (=<<) :: forall a b m. (Prelude.Bind m) => (a -> m b) -> m a -> m b


    (>=>) :: forall a b c m. (Prelude.Bind m) => (a -> m b) -> (b -> m c) -> a -> m c


    ifM :: forall a m. (Prelude.Bind m) => m Boolean -> m a -> m a -> m a


    join :: forall a m. (Prelude.Bind m) => m (m a) -> m a


## Module Control.Apply

### Values


    (*>) :: forall a b f. (Prelude.Apply f) => f a -> f b -> f b


    (<*) :: forall a b f. (Prelude.Apply f) => f a -> f b -> f a


    forever :: forall a b f. (Prelude.Apply f) => f a -> f b


    lift2 :: forall a b c f. (Prelude.Apply f) => (a -> b -> c) -> f a -> f b -> f c


    lift3 :: forall a b c d f. (Prelude.Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d


    lift4 :: forall a b c d e f. (Prelude.Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e


    lift5 :: forall a b c d e f g. (Prelude.Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g


## Module Control.Alt

### Type Classes


    class (Prelude.Functor f) <= Alt f where

      (<|>) :: forall a. f a -> f a -> f a


## Module Control.Plus

### Type Classes


    class (Control.Alt.Alt f) <= Plus f where

      empty :: forall a. f a


## Module Control.Alternative

### Type Classes


    class (Prelude.Applicative f, Control.Plus.Plus f) <= Alternative f where


### Values


    many :: forall f a. (Control.Alternative.Alternative f, Control.Lazy.Lazy1 f) => f a -> f [a]


    some :: forall f a. (Control.Alternative.Alternative f, Control.Lazy.Lazy1 f) => f a -> f [a]


## Module Control.MonadPlus

### Type Classes


    class (Prelude.Monad m, Control.Alternative.Alternative m) <= MonadPlus m where


### Values


    guard :: forall m. (Control.MonadPlus.MonadPlus m) => Boolean -> m Prelude.Unit


## Module Data.Either

### Types


    data Either (a :: *) (b :: *) where
      Left :: a -> Either (a :: *) (b :: *)
      Right :: b -> Either (a :: *) (b :: *)


### Values


    either :: forall a b c. (a -> c) -> (b -> c) -> Data.Either.Either a b -> c


    isLeft :: forall a b. Data.Either.Either a b -> Boolean


    isRight :: forall a b. Data.Either.Either a b -> Boolean


## Module Data.Either.Nested

### Values


    choice10 :: forall a b c d e f g h i j z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c (Data.Either.Either d (Data.Either.Either e (Data.Either.Either f (Data.Either.Either g (Data.Either.Either h (Data.Either.Either i j)))))))) -> z


    choice2 :: forall a b z. (a -> z) -> (b -> z) -> Data.Either.Either a b -> z


    choice3 :: forall a b c z. (a -> z) -> (b -> z) -> (c -> z) -> Data.Either.Either a (Data.Either.Either b c) -> z


    choice4 :: forall a b c d z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c d)) -> z


    choice5 :: forall a b c d e z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c (Data.Either.Either d e))) -> z


    choice6 :: forall a b c d e f z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c (Data.Either.Either d (Data.Either.Either e f)))) -> z


    choice7 :: forall a b c d e f g z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c (Data.Either.Either d (Data.Either.Either e (Data.Either.Either f g))))) -> z


    choice8 :: forall a b c d e f g h z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c (Data.Either.Either d (Data.Either.Either e (Data.Either.Either f (Data.Either.Either g h)))))) -> z


    choice9 :: forall a b c d e f g h i z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> Data.Either.Either a (Data.Either.Either b (Data.Either.Either c (Data.Either.Either d (Data.Either.Either e (Data.Either.Either f (Data.Either.Either g (Data.Either.Either h i))))))) -> z


## Module Data.Maybe

### Types


    data Maybe (a :: *) where
      Nothing :: Maybe (a :: *)
      Just :: a -> Maybe (a :: *)


### Values


    fromMaybe :: forall a. a -> Data.Maybe.Maybe a -> a


    isJust :: forall a. Data.Maybe.Maybe a -> Boolean


    isNothing :: forall a. Data.Maybe.Maybe a -> Boolean


    maybe :: forall a b. b -> (a -> b) -> Data.Maybe.Maybe a -> b


## Module Data.Array

### Values


    (!!) :: forall a. [a] -> Number -> Data.Maybe.Maybe a


    (..) :: Number -> Number -> [Number]


    (\\) :: forall a. (Prelude.Eq a) => [a] -> [a] -> [a]


    append :: forall a. [a] -> [a] -> [a]


    catMaybes :: forall a. [Data.Maybe.Maybe a] -> [a]


    concat :: forall a. [[a]] -> [a]


    concatMap :: forall a b. (a -> [b]) -> [a] -> [b]


    delete :: forall a. (Prelude.Eq a) => a -> [a] -> [a]


    deleteAt :: forall a. Number -> Number -> [a] -> [a]


    deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]


    drop :: forall a. Number -> [a] -> [a]


    dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]


    elemIndex :: forall a. (Prelude.Eq a) => a -> [a] -> Number


    elemLastIndex :: forall a. (Prelude.Eq a) => a -> [a] -> Number


    filter :: forall a. (a -> Boolean) -> [a] -> [a]


    findIndex :: forall a. (a -> Boolean) -> [a] -> Number


    findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number


    group :: forall a. (Prelude.Eq a) => [a] -> [[a]]


    group' :: forall a. (Prelude.Ord a) => [a] -> [[a]]


    groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]


    head :: forall a. [a] -> Data.Maybe.Maybe a


    init :: forall a. [a] -> Data.Maybe.Maybe [a]


    insertAt :: forall a. Number -> a -> [a] -> [a]


    intersect :: forall a. (Prelude.Eq a) => [a] -> [a] -> [a]


    intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]


    last :: forall a. [a] -> Data.Maybe.Maybe a


    length :: forall a. [a] -> Number


    map :: forall a b. (a -> b) -> [a] -> [b]


    mapMaybe :: forall a b. (a -> Data.Maybe.Maybe b) -> [a] -> [b]


    nub :: forall a. (Prelude.Eq a) => [a] -> [a]


    nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]


    null :: forall a. [a] -> Boolean


    range :: Number -> Number -> [Number]


    reverse :: forall a. [a] -> [a]


    singleton :: forall a. a -> [a]


    snoc :: forall a. [a] -> a -> [a]


    sort :: forall a. (Prelude.Ord a) => [a] -> [a]


    sortBy :: forall a. (a -> a -> Prelude.Ordering) -> [a] -> [a]


    span :: forall a. (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }


    tail :: forall a. [a] -> Data.Maybe.Maybe [a]


    take :: forall a. Number -> [a] -> [a]

      | Performs a sorting first.

    takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]


    updateAt :: forall a. Number -> a -> [a] -> [a]


    zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]


## Module Data.Array.ST

### Types


    type Assoc (a :: *) = { value :: a, index :: Number }


    data STArray :: * -> * -> *


### Values


    emptySTArray :: forall a h r. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Array.ST.STArray h a)


    freeze :: forall a h r. Data.Array.ST.STArray h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) [a]


    peekSTArray :: forall a h r. Data.Array.ST.STArray h a -> Number -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Maybe.Maybe a)


    pokeSTArray :: forall a h r. Data.Array.ST.STArray h a -> Number -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) Boolean


    pushAllSTArray :: forall a h r. Data.Array.ST.STArray h a -> [a] -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) Number


    pushSTArray :: forall a h r. Data.Array.ST.STArray h a -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) Number


    runSTArray :: forall a r. (forall h. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Array.ST.STArray h a)) -> Control.Monad.Eff.Eff r [a]


    spliceSTArray :: forall a h r. Data.Array.ST.STArray h a -> Number -> Number -> [a] -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) [a]


    thaw :: forall a h r. [a] -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Array.ST.STArray h a)


    toAssocArray :: forall a h r. Data.Array.ST.STArray h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) [Data.Array.ST.Assoc a]


## Module Data.Maybe.Unsafe

### Values


    fromJust :: forall a. Data.Maybe.Maybe a -> a


## Module Data.Array.Unsafe

### Values


    head :: forall a. [a] -> a


    init :: forall a. [a] -> [a]


    last :: forall a. [a] -> a


    tail :: forall a. [a] -> [a]


## Module Data.Monoid

### Type Classes


    class (Prelude.Semigroup m) <= Monoid m where

      mempty :: m


## Module Data.Monoid.All

### Types


    newtype All where
      All :: Boolean -> All


### Values


    runAll :: Data.Monoid.All.All -> Boolean


## Module Data.Monoid.Any

### Types


    newtype Any where
      Any :: Boolean -> Any


### Values


    runAny :: Data.Monoid.Any.Any -> Boolean


## Module Data.Monoid.Dual

### Types


    newtype Dual (a :: *) where
      Dual :: a -> Dual (a :: *)


### Values


    runDual :: forall a. Data.Monoid.Dual.Dual a -> a


## Module Data.Monoid.Endo

### Types


    newtype Endo (a :: *) where
      Endo :: (a -> a) -> Endo (a :: *)


### Values


    runEndo :: forall a. Data.Monoid.Endo.Endo a -> a -> a


## Module Data.Monoid.Product

### Types


    newtype Product where
      Product :: Number -> Product


### Values


    runProduct :: Data.Monoid.Product.Product -> Number


## Module Data.Monoid.Sum

### Types


    newtype Sum where
      Sum :: Number -> Sum


### Values


    runSum :: Data.Monoid.Sum.Sum -> Number


## Module Data.Tuple

### Types


    data Tuple (a :: *) (b :: *) where
      Tuple :: a -> b -> Tuple (a :: *) (b :: *)


### Values


    curry :: forall a b c. (Data.Tuple.Tuple a b -> c) -> a -> b -> c


    fst :: forall a b. Data.Tuple.Tuple a b -> a


    snd :: forall a b. Data.Tuple.Tuple a b -> b


    swap :: forall a b. Data.Tuple.Tuple a b -> Data.Tuple.Tuple b a


    uncurry :: forall a b c. (a -> b -> c) -> Data.Tuple.Tuple a b -> c


    unzip :: forall a b. [Data.Tuple.Tuple a b] -> Data.Tuple.Tuple [a] [b]


    zip :: forall a b. [a] -> [b] -> [Data.Tuple.Tuple a b]


## Module Data.Tuple.Nested

### Values


    (/\) :: forall a b. a -> b -> Data.Tuple.Tuple a b


    con10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c (Data.Tuple.Tuple d (Data.Tuple.Tuple e (Data.Tuple.Tuple f (Data.Tuple.Tuple g (Data.Tuple.Tuple h (Data.Tuple.Tuple i j)))))))) -> z


    con2 :: forall a b z. (a -> b -> z) -> Data.Tuple.Tuple a b -> z


    con3 :: forall a b c z. (a -> b -> c -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b c) -> z


    con4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c d)) -> z


    con5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c (Data.Tuple.Tuple d e))) -> z


    con6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c (Data.Tuple.Tuple d (Data.Tuple.Tuple e f)))) -> z


    con7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c (Data.Tuple.Tuple d (Data.Tuple.Tuple e (Data.Tuple.Tuple f g))))) -> z


    con8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c (Data.Tuple.Tuple d (Data.Tuple.Tuple e (Data.Tuple.Tuple f (Data.Tuple.Tuple g h)))))) -> z


    con9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Data.Tuple.Tuple a (Data.Tuple.Tuple b (Data.Tuple.Tuple c (Data.Tuple.Tuple d (Data.Tuple.Tuple e (Data.Tuple.Tuple f (Data.Tuple.Tuple g (Data.Tuple.Tuple h i))))))) -> z


## Module Data.Monoid.First

### Types


    newtype First (a :: *) where
      First :: Data.Maybe.Maybe a -> First (a :: *)


### Values


    runFirst :: forall a. Data.Monoid.First.First a -> Data.Maybe.Maybe a


## Module Data.Foldable

### Type Classes


    class Foldable f where

      foldr :: forall a b. (a -> b -> b) -> b -> f a -> b

      foldl :: forall a b. (b -> a -> b) -> b -> f a -> b

      foldMap :: forall a m. (Data.Monoid.Monoid m) => (a -> m) -> f a -> m


### Values


    all :: forall a f. (Data.Foldable.Foldable f) => (a -> Boolean) -> f a -> Boolean


    and :: forall f. (Data.Foldable.Foldable f) => f Boolean -> Boolean


    any :: forall a f. (Data.Foldable.Foldable f) => (a -> Boolean) -> f a -> Boolean


    elem :: forall a f. (Prelude.Eq a, Data.Foldable.Foldable f) => a -> f a -> Boolean


    find :: forall a f. (Data.Foldable.Foldable f) => (a -> Boolean) -> f a -> Data.Maybe.Maybe a


    fold :: forall f m. (Data.Foldable.Foldable f, Data.Monoid.Monoid m) => f m -> m


    foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b


    foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b


    for_ :: forall a b f m. (Prelude.Applicative m, Data.Foldable.Foldable f) => f a -> (a -> m b) -> m Prelude.Unit


    intercalate :: forall f m. (Data.Foldable.Foldable f, Data.Monoid.Monoid m) => m -> f m -> m


    lookup :: forall a b f. (Prelude.Eq a, Data.Foldable.Foldable f) => a -> f (Data.Tuple.Tuple a b) -> Data.Maybe.Maybe b


    mconcat :: forall f m. (Data.Foldable.Foldable f, Data.Monoid.Monoid m) => f m -> m


    notElem :: forall a f. (Prelude.Eq a, Data.Foldable.Foldable f) => a -> f a -> Boolean


    or :: forall f. (Data.Foldable.Foldable f) => f Boolean -> Boolean


    product :: forall f. (Data.Foldable.Foldable f) => f Number -> Number


    sequence_ :: forall a f m. (Prelude.Applicative m, Data.Foldable.Foldable f) => f (m a) -> m Prelude.Unit


    sum :: forall f. (Data.Foldable.Foldable f) => f Number -> Number


    traverse_ :: forall a b f m. (Prelude.Applicative m, Data.Foldable.Foldable f) => (a -> m b) -> f a -> m Prelude.Unit


## Module Data.Monoid.Last

### Types


    newtype Last (a :: *) where
      Last :: Data.Maybe.Maybe a -> Last (a :: *)


### Values


    runLast :: forall a. Data.Monoid.Last.Last a -> Data.Maybe.Maybe a


## Module Data.Traversable

### Type Classes


    class (Prelude.Functor t, Data.Foldable.Foldable t) <= Traversable t where

      traverse :: forall a b m. (Prelude.Applicative m) => (a -> m b) -> t a -> m (t b)

      sequence :: forall a m. (Prelude.Applicative m) => t (m a) -> m (t a)


### Values


    for :: forall a b m t. (Prelude.Applicative m, Data.Traversable.Traversable t) => t a -> (a -> m b) -> m (t b)


    mapAccumL :: forall a b s f. (Data.Traversable.Traversable f) => (s -> a -> Data.Tuple.Tuple s b) -> s -> f a -> Data.Tuple.Tuple s (f b)


    mapAccumR :: forall a b s f. (Data.Traversable.Traversable f) => (s -> a -> Data.Tuple.Tuple s b) -> s -> f a -> Data.Tuple.Tuple s (f b)


    scanl :: forall a b f. (Data.Traversable.Traversable f) => (b -> a -> b) -> b -> f a -> f b


    scanr :: forall a b f. (Data.Traversable.Traversable f) => (a -> b -> b) -> b -> f a -> f b


    zipWithA :: forall m a b c. (Prelude.Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]


## Module Main

### Types


    type Coordinate = { x :: Number, y :: Number }

      right arrow, d
      px
      NOTE: subtracts 4 pixels for mario sprite offset
      left arrow, a
      up arrow, w

    type Inputs = { right :: Boolean, left :: Boolean, jump :: Boolean }


### Values


    combineKeyPresses :: [Signal.Signal Boolean] -> Signal.Signal Boolean


    getMario :: forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) DOM.Node


    groundHeight :: Number


    jumpKeyCodes :: [Number]


    leftKeyCodes :: [Number]


    main :: forall t6202. Control.Monad.Eff.Eff (timer :: Control.Timer.Timer | t6202) Prelude.Unit


    mkInputs :: Boolean -> Boolean -> Boolean -> Main.Inputs


    offsetGround :: Number -> Main.Coordinate -> Main.Coordinate


    onDOMContentLoaded :: forall eff a. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) a -> Control.Monad.Eff.Eff eff Prelude.Unit


    rightKeyCodes :: [Number]


    updatePosition :: DOM.Node -> Main.Coordinate -> (forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) Prelude.Unit)


    updateSprite :: DOM.Node -> String -> (forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) Prelude.Unit)



