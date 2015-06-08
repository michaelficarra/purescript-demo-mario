# Module Documentation

## Module Prelude

#### `Unit`

``` purescript
newtype Unit
  = Unit {  }
```

Attaches an element to the front of an array, creating a new array.

```purescript
cons 1 [2, 3, 4] = [1, 2, 3, 4]
```

Note, the running time of this function is `O(n)`.
The `BoolLike` type class identifies types which support Boolean operations.

`BoolLike` instances are required to satisfy the laws of a _Boolean algebra_.

The `Bind` type class extends the [`Apply`](#apply) type class with a "bind" operation `(>>=)` which composes computations
in sequence, using the return value of one computation to determine the next computation.

The `>>=` operator can also be expressed using `do` notation, as follows:

```purescript
x >>= f = do y <- x
             f y
```

where the function argument of `f` is given the name `y`.

`Bind` instances should satisfy the following law:

- Associativity: `(x >>= f) >>= g = x >>= (\k => f k >>= g)`

Or, expressed using `do` notation:

- Associativity: `do { z <- do { y <- x ; f y } ; g z } = do { k <- x ; do { y <- f k ; g y } }`

Associativity tells us that we can regroup operations which use do-notation, so that we can unambiguously write, for example:

```purescript
do x <- m1
   y <- m2 x
   m3 x y
```
The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`
- Negation: `x /= y = not (x == y)`

`(/=)` may be implemented in terms of `(==)`, but it might give a performance improvement to implement it separately.
The `Semigroup` type class identifies an associative operation on a type.

`Semigroup` instances are required to satisfy the following law:

- Associativity: `(x <> y) <> z = x <> (y <> z)`

For example, the `String` type is an instance of `Semigroup`, where `(<>)` is defined to be string concatenation.
A `Semigroupoid` is similar to a [`Category`](#category) but does not require an identity
element `id`, just composable morphisms.

`Semigroupoid`s should obey the following rule:

- Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`

One example of a `Semigroupoid` is the function type constructor `(->)`, with `(<<<)` defined
as function composition.
Forwards composition, or `(<<<)` with its arguments reversed.
The `Apply` class provides the `(<*>)` which is used to apply a function to an argument under a type constructor.

`Apply` can be used to lift functions of two or more arguments to work on values wrapped with the type constructor `f`.
It might also be understood in terms of the `lift2` function:

```purescript
lift2 :: forall f a b c. (Apply f) => (a -> b -> c) -> f a -> f b -> f c
lift2 f a b = f <$> a <*> b
```

`(<*>)` is recovered from `lift2` as `lift2 ($)`. That is, `(<*>)` lifts the function application operator `($)` to arguments
wrapped with the type constructor `f`.

`Apply` instances should satisfy the following law:

- Associative Composition: `(<<<) <$> f <*> g <*> h = f <*> (g <*> h)`

Formally, `Apply` represents a strong lax semi-monoidal endofunctor.
A `Functor` is a type constructor which supports a mapping operation `(<$>)`.

`(<$>)` can be used to turn functions `a -> b` into functions `f a -> f b` whose argument and return
types use the type constructor `f` to represent some computational context.

`Functor` instances should satisfy the following laws:

- Identity: `(<$>) id = id`
- Composition: `(<$>) (f <<< g) = (f <$>) <<< (g <$>)`

`(<#>)` is `(<$>)` with its arguments reversed. For example:

```purescript
[1, 2, 3] <#> \n -> n * n
```
An infix alias for `cons`.

Note, the running time of this function is `O(n)`.
The `Eq` type class represents types which support decidable equality.

`Eq` instances should satisfy the following laws:

- Reflexivity: `x == x = true`
- Symmetry: `x == y = y == x`
- Transitivity: if `x == y` and `y == z` then `x == z`
- Negation: `x /= y = not (x == y)`

`(/=)` may be implemented in terms of `(==)`, but it might give a performance improvement to implement it separately.
Addition, multiplication, modulo operation and division, satisfying:

- ```a / b * b + (a `mod` b) = a```

The `Bits` type class identifies types which support bitwise operations.
The `Bits` type class identifies types which support bitwise operations.
The `Bits` type class identifies types which support bitwise operations.
Addition, multiplication, and subtraction.

Has the same laws as `Semiring` but additionally satisfying:

- `a` is an abelian group under addition

`(++)` is an alias for `(<>)`.
Addition and multiplication, satisfying the following laws:

- `a` is a commutative monoid under addition
- `a` is a monoid under multiplication
- multiplication distributes over addition
- multiplication by `zero` annihilates `a`

Addition and multiplication, satisfying the following laws:

- `a` is a commutative monoid under addition
- `a` is a monoid under multiplication
- multiplication distributes over addition
- multiplication by `zero` annihilates `a`

The `BoolLike` type class identifies types which support Boolean operations.

`BoolLike` instances are required to satisfy the laws of a _Boolean algebra_.

Applies a function to its argument

```purescript
length $ groupBy productCategory $ filter isInStock products
```

is equivalent to

```purescript
length (groupBy productCategory (filter isInStock (products)))
```

`($)` is different from [`(#)`](#-2) because it is right-infix instead of left, so
`a $ b $ c $ d x` = `a $ (b $ (c $ (d $ x)))` = `a (b (c (d x)))`

Applies a function to its argument

```purescript
products # groupBy productCategory # filter isInStock # length
```

is equivalent to

```purescript
length (groupBy productCategory (filter isInStock (products)))
```

`(#)` is different from [`($)`](#-1) because it is left-infix instead of right, so
`x # a # b # c # d` = `(((x # a) # b) # c) # d` = `d (c (b (a x)))`

The `Bits` type class identifies types which support bitwise operations.
Addition and multiplication, satisfying the following laws:

- `a` is a commutative monoid under addition
- `a` is a monoid under multiplication
- multiplication distributes over addition
- multiplication by `zero` annihilates `a`

`unit` is the sole inhabitant of the `Unit` type.
The `Bits` type class identifies types which support bitwise operations.
The `Show` type class represents those types which can be converted into a human-readable `String` representation.

While not required, it is recommended that for any expression `x`, the string `show x` be executable PureScript code
which evaluates to the same value as the expression `x`.
The `Bits` type class identifies types which support bitwise operations.
The `Applicative` type class extends the [`Apply`](#apply) type class with a `pure` function, which can be used to
create values of type `f a` from values of type `a`.

Where [`Apply`](#apply) provides the ability to lift functions of two or more arguments to functions whose arguments are wrapped using `f`,
and [`Functor`](#functor) provides the ability to lift functions of one argument, `pure` can be seen as the function which lifts functions of
_zero_ arguments. That is, `Applicative` functors support a lifting operation for any number of function arguments.

`Applicative` instances should satisfy the following laws:

- Identity: `(pure id) <*> v = v`
- Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
- Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
- Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`

`return` is an alias for `pure`.
An alias for `true`, which can be useful in guard clauses:

```purescript
max x y | x >= y = x
        | otherwise = y
```

Addition and multiplication, satisfying the following laws:

- `a` is a commutative monoid under addition
- `a` is a monoid under multiplication
- multiplication distributes over addition
- multiplication by `zero` annihilates `a`

The `BoolLike` type class identifies types which support Boolean operations.

`BoolLike` instances are required to satisfy the laws of a _Boolean algebra_.

Addition, multiplication, modulo operation and division, satisfying:

- ```a / b * b + (a `mod` b) = a```

`liftM1` provides a default implementation of `(<$>)` for any [`Monad`](#monad),
without using `(<$>)` as provided by the [`Functor`](#functor)-[`Monad`](#monad) superclass relationship.

`liftM1` can therefore be used to write [`Functor`](#functor) instances as follows:

```purescript
instance functorF :: Functor F where
  (<$>) = liftM1
```
`liftA1` provides a default implementation of `(<$>)` for any [`Applicative`](#applicative) functor,
without using `(<$>)` as provided by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass relationship.

`liftA1` can therefore be used to write [`Functor`](#functor) instances as follows:

```purescript
instance functorF :: Functor F where
  (<$>) = liftA1
```
`Category`s consist of objects and composable morphisms between them, and as such are
[`Semigroupoids`](#semigroupoid), but unlike `semigroupoids` must have an identity element.

`Category`s should obey the following rules.

- Left Identity: `id <<< p = p`
- Right Identity: `p <<< id = p`

Flips the order of the arguments to a function of two arguments.

```purescript
flip const 1 2 = const 2 1 = 2
```

Returns its first argument and ignores its second.

```purescript
const 1 "hello" = 1
```

The `void` function is used to ignore the type wrapped by a [`Functor`](#functor), replacing it with `Unit` and
keeping only the type information provided by the type constructor itself.

`void` is often useful when using `do` notation to change the return type of a monadic computation:

```purescript
main = forE 1 10 \n -> void do
  print n
  print (n * n)
```
The `Bits` type class identifies types which support bitwise operations.
The `Ord` type class represents types which support comparisons.

`Ord` instances should satisfy the laws of _partially orderings_:

- Reflexivity: `a <= a`
- Antisymmetry: if `a <= b` and `b <= a` then `a = b`
- Transitivity: if `a <= b` and `b <= c` then `a <= c`

Test whether one value is _strictly less than_ another.
Test whether one value is _non-strictly less than_ another.
Test whether one value is _strictly greater than_ another.
Test whether one value is _non-strictly greater than_ another.
This function returns its first argument, and can be used to assert type equalities.
This can be useful when types are otherwise ambiguous.

```purescript
main = print $ [] `asTypeOf` [0]
```

If instead, we had written `main = print []`, the type of the argument `[]` would have
been ambiguous, resulting in a compile-time error.
`ap` provides a default implementation of `(<*>)` for any [`Monad`](#monad),
without using `(<*>)` as provided by the [`Apply`](#apply)-[`Monad`](#monad) superclass relationship.

`ap` can therefore be used to write [`Apply`](#apply) instances as follows:

```purescript
instance applyF :: Apply F where
  (<*>) = ap
```

#### `Ordering`

``` purescript
data Ordering
  = LT 
  | GT 
  | EQ 
```


#### `Semigroup`

``` purescript
class Semigroup a where
  (<>) :: a -> a -> a
```


#### `BoolLike`

``` purescript
class BoolLike b where
  (&&) :: b -> b -> b
  (||) :: b -> b -> b
  not :: b -> b
```


#### `Bits`

``` purescript
class Bits b where
  (.&.) :: b -> b -> b
  (.|.) :: b -> b -> b
  (.^.) :: b -> b -> b
  shl :: b -> Number -> b
  shr :: b -> Number -> b
  zshr :: b -> Number -> b
  complement :: b -> b
```


#### `Ord`

``` purescript
class (Prelude.Eq a) <= Ord a where
  compare :: a -> a -> Prelude.Ordering
```


#### `Eq`

``` purescript
class Eq a where
  (==) :: a -> a -> Boolean
  (/=) :: a -> a -> Boolean
```


#### `Num`

``` purescript
class (Prelude.DivisionRing a) <= Num a where
```


#### `DivisionRing`

``` purescript
class (Prelude.Ring a, Prelude.ModuloSemiring a) <= DivisionRing a where
```


#### `Ring`

``` purescript
class (Prelude.Semiring a) <= Ring a where
  (-) :: a -> a -> a
```


#### `ModuloSemiring`

``` purescript
class (Prelude.Semiring a) <= ModuloSemiring a where
  (/) :: a -> a -> a
  mod :: a -> a -> a
```


#### `Semiring`

``` purescript
class Semiring a where
  (+) :: a -> a -> a
  zero :: a
  (*) :: a -> a -> a
  one :: a
```


#### `Monad`

``` purescript
class (Prelude.Applicative m, Prelude.Bind m) <= Monad m where
```


#### `Bind`

``` purescript
class (Prelude.Apply m) <= Bind m where
  (>>=) :: forall a b. m a -> (a -> m b) -> m b
```


#### `Applicative`

``` purescript
class (Prelude.Apply f) <= Applicative f where
  pure :: forall a. a -> f a
```


#### `Apply`

``` purescript
class (Prelude.Functor f) <= Apply f where
  (<*>) :: forall a b. f (a -> b) -> f a -> f b
```


#### `Functor`

``` purescript
class Functor f where
  (<$>) :: forall a b. (a -> b) -> f a -> f b
```


#### `Show`

``` purescript
class Show a where
  show :: a -> String
```


#### `Category`

``` purescript
class (Prelude.Semigroupoid a) <= Category a where
  id :: forall t. a t t
```


#### `Semigroupoid`

``` purescript
class Semigroupoid a where
  (<<<) :: forall b c d. a c d -> a b c -> a b d
```


#### `unit`

``` purescript
unit :: Prelude.Unit
```


#### `(++)`

``` purescript
(++) :: forall s. (Prelude.Semigroup s) => s -> s -> s
```


#### `(>=)`

``` purescript
(>=) :: forall a. (Prelude.Ord a) => a -> a -> Boolean
```


#### `(<=)`

``` purescript
(<=) :: forall a. (Prelude.Ord a) => a -> a -> Boolean
```


#### `(>)`

``` purescript
(>) :: forall a. (Prelude.Ord a) => a -> a -> Boolean
```


#### `(<)`

``` purescript
(<) :: forall a. (Prelude.Ord a) => a -> a -> Boolean
```


#### `negate`

``` purescript
negate :: forall a. (Prelude.Ring a) => a -> a
```


#### `(%)`

``` purescript
(%) :: Number -> Number -> Number
```


#### `ap`

``` purescript
ap :: forall m a b. (Prelude.Monad m) => m (a -> b) -> m a -> m b
```


#### `liftM1`

``` purescript
liftM1 :: forall m a b. (Prelude.Monad m) => (a -> b) -> m a -> m b
```


#### `return`

``` purescript
return :: forall m a. (Prelude.Monad m) => a -> m a
```


#### `liftA1`

``` purescript
liftA1 :: forall f a b. (Prelude.Applicative f) => (a -> b) -> f a -> f b
```


#### `void`

``` purescript
void :: forall f a. (Prelude.Functor f) => f a -> f Prelude.Unit
```


#### `(<#>)`

``` purescript
(<#>) :: forall f a b. (Prelude.Functor f) => f a -> (a -> b) -> f b
```


#### `cons`

``` purescript
cons :: forall a. a -> [a] -> [a]
```


#### `(:)`

``` purescript
(:) :: forall a. a -> [a] -> [a]
```


#### `(#)`

``` purescript
(#) :: forall a b. a -> (a -> b) -> b
```


#### `($)`

``` purescript
($) :: forall a b. (a -> b) -> a -> b
```


#### `(>>>)`

``` purescript
(>>>) :: forall a b c d. (Prelude.Semigroupoid a) => a b c -> a c d -> a b d
```


#### `asTypeOf`

``` purescript
asTypeOf :: forall a. a -> a -> a
```


#### `const`

``` purescript
const :: forall a b. a -> b -> a
```


#### `flip`

``` purescript
flip :: forall a b c. (a -> b -> c) -> b -> a -> c
```


#### `otherwise`

``` purescript
otherwise :: Boolean
```



## Module Prelude.Unsafe

#### `unsafeIndex`

``` purescript
unsafeIndex :: forall a. [a] -> Number -> a
```

Find the element of an array at the specified index.

Note: this function can cause unpredictable failure at runtime if the index is out-of-bounds.


## Module Math

#### `Radians`

``` purescript
type Radians = Number
```

An alias to make types in this module more explicit.
Returns the absolute value of the argument.
Returns the inverse cosine of the argument.
Returns the inverse sine of the argument.
Returns the inverse tangent of the argument.
Four-quadrant tangent inverse. Given the arguments `y` and `x`, returns
the inverse tangent of `y / x`, where the signs of both arguments are used
to determine the sign of the result.
If the first argument is negative, the result will be negative.
The result is the angle between the positive x axis and  a point `(x, y)`.
Returns the smallest integer not smaller than the argument.
Returns the cosine of the argument.
Returns `e` exponentiated to the power of the argument.
Returns the largest integer not larger than the argument.
Returns the natural logarithm of a number.
Returns the largest of two numbers.
Returns the smallest of two numbers.
Return  the first argument exponentiated to the power of the second argument.
Returns the integer closest to the argument.
Returns the sine of the argument.
Returns the square root of the argument.
Returns the tangent of the argument.
The base of natural logarithms, *e*, around 2.71828.
The natural logarithm of 2, around 0.6931.
The natural logarithm of 10, around 2.3025.
The base 2 logarithm of `e`, around 1.4426.
Base 10 logarithm of `e`, around 0.43429.
The ratio of the circumference of a circle to its diameter, around 3.14159.
The Square root of one half, around 0.707107.
The square root of two, around 1.41421.

#### `sqrt2`

``` purescript
sqrt2 :: Number
```


#### `sqrt1_2`

``` purescript
sqrt1_2 :: Number
```


#### `pi`

``` purescript
pi :: Number
```


#### `log10e`

``` purescript
log10e :: Number
```


#### `log2e`

``` purescript
log2e :: Number
```


#### `ln10`

``` purescript
ln10 :: Number
```


#### `ln2`

``` purescript
ln2 :: Number
```


#### `e`

``` purescript
e :: Number
```


#### `tan`

``` purescript
tan :: Math.Radians -> Number
```


#### `sqrt`

``` purescript
sqrt :: Number -> Number
```


#### `sin`

``` purescript
sin :: Math.Radians -> Number
```


#### `round`

``` purescript
round :: Number -> Number
```


#### `pow`

``` purescript
pow :: Number -> Number -> Number
```


#### `min`

``` purescript
min :: Number -> Number -> Number
```


#### `max`

``` purescript
max :: Number -> Number -> Number
```


#### `log`

``` purescript
log :: Number -> Number
```


#### `floor`

``` purescript
floor :: Number -> Number
```


#### `exp`

``` purescript
exp :: Number -> Number
```


#### `cos`

``` purescript
cos :: Math.Radians -> Number
```


#### `ceil`

``` purescript
ceil :: Number -> Number
```


#### `atan2`

``` purescript
atan2 :: Number -> Number -> Math.Radians
```


#### `atan`

``` purescript
atan :: Number -> Math.Radians
```


#### `asin`

``` purescript
asin :: Number -> Math.Radians
```


#### `acos`

``` purescript
acos :: Number -> Math.Radians
```


#### `abs`

``` purescript
abs :: Number -> Number
```



## Module Data.Function

#### `Fn10`

``` purescript
data Fn10 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
```

A function of zero arguments
A function of one argument
A function of two arguments
A function of three arguments
A function of four arguments
A function of five arguments
A function of six arguments
A function of seven arguments
A function of eight arguments
A function of nine arguments
A function of ten arguments
Create a function of no arguments
Create a function of one argument
Create a function of two arguments from a curried function
Create a function of three arguments from a curried function
Create a function of four arguments from a curried function
Create a function of five arguments from a curried function
Create a function of six arguments from a curried function
Create a function of seven arguments from a curried function
Create a function of eight arguments from a curried function
Create a function of nine arguments from a curried function
Create a function of ten arguments from a curried function
Apply a function of no arguments
Apply a function of one argument
Apply a function of two arguments
Apply a function of three arguments
Apply a function of four arguments
Apply a function of five arguments
Apply a function of six arguments
Apply a function of seven arguments
Apply a function of eight arguments
Apply a function of nine arguments
Apply a function of ten arguments
The `on` function is used to change the domain of a binary operator.

For example, we can create a function which compares two records based on the values of their `x` properties:

```purescript
compareX :: forall r. { x :: Number | r } -> { x :: Number | r } -> Ordering
compareX = compare `on` _.x
```

#### `Fn9`

``` purescript
data Fn9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn8`

``` purescript
data Fn8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn7`

``` purescript
data Fn7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn6`

``` purescript
data Fn6 :: * -> * -> * -> * -> * -> * -> * -> *
```


#### `Fn5`

``` purescript
data Fn5 :: * -> * -> * -> * -> * -> * -> *
```


#### `Fn4`

``` purescript
data Fn4 :: * -> * -> * -> * -> * -> *
```


#### `Fn3`

``` purescript
data Fn3 :: * -> * -> * -> * -> *
```


#### `Fn2`

``` purescript
data Fn2 :: * -> * -> * -> *
```


#### `Fn1`

``` purescript
data Fn1 :: * -> * -> *
```


#### `Fn0`

``` purescript
data Fn0 :: * -> *
```


#### `runFn10`

``` purescript
runFn10 :: forall a b c d e f g h i j k. Data.Function.Fn10 a b c d e f g h i j k -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k
```


#### `runFn9`

``` purescript
runFn9 :: forall a b c d e f g h i j. Data.Function.Fn9 a b c d e f g h i j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j
```


#### `runFn8`

``` purescript
runFn8 :: forall a b c d e f g h i. Data.Function.Fn8 a b c d e f g h i -> a -> b -> c -> d -> e -> f -> g -> h -> i
```


#### `runFn7`

``` purescript
runFn7 :: forall a b c d e f g h. Data.Function.Fn7 a b c d e f g h -> a -> b -> c -> d -> e -> f -> g -> h
```


#### `runFn6`

``` purescript
runFn6 :: forall a b c d e f g. Data.Function.Fn6 a b c d e f g -> a -> b -> c -> d -> e -> f -> g
```


#### `runFn5`

``` purescript
runFn5 :: forall a b c d e f. Data.Function.Fn5 a b c d e f -> a -> b -> c -> d -> e -> f
```


#### `runFn4`

``` purescript
runFn4 :: forall a b c d e. Data.Function.Fn4 a b c d e -> a -> b -> c -> d -> e
```


#### `runFn3`

``` purescript
runFn3 :: forall a b c d. Data.Function.Fn3 a b c d -> a -> b -> c -> d
```


#### `runFn2`

``` purescript
runFn2 :: forall a b c. Data.Function.Fn2 a b c -> a -> b -> c
```


#### `runFn1`

``` purescript
runFn1 :: forall a b. Data.Function.Fn1 a b -> a -> b
```


#### `runFn0`

``` purescript
runFn0 :: forall a. Data.Function.Fn0 a -> a
```


#### `mkFn10`

``` purescript
mkFn10 :: forall a b c d e f g h i j k. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> Data.Function.Fn10 a b c d e f g h i j k
```


#### `mkFn9`

``` purescript
mkFn9 :: forall a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> Data.Function.Fn9 a b c d e f g h i j
```


#### `mkFn8`

``` purescript
mkFn8 :: forall a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Data.Function.Fn8 a b c d e f g h i
```


#### `mkFn7`

``` purescript
mkFn7 :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> Data.Function.Fn7 a b c d e f g h
```


#### `mkFn6`

``` purescript
mkFn6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> Data.Function.Fn6 a b c d e f g
```


#### `mkFn5`

``` purescript
mkFn5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Data.Function.Fn5 a b c d e f
```


#### `mkFn4`

``` purescript
mkFn4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Data.Function.Fn4 a b c d e
```


#### `mkFn3`

``` purescript
mkFn3 :: forall a b c d. (a -> b -> c -> d) -> Data.Function.Fn3 a b c d
```


#### `mkFn2`

``` purescript
mkFn2 :: forall a b c. (a -> b -> c) -> Data.Function.Fn2 a b c
```


#### `mkFn1`

``` purescript
mkFn1 :: forall a b. (a -> b) -> Data.Function.Fn1 a b
```


#### `mkFn0`

``` purescript
mkFn0 :: forall a. (Prelude.Unit -> a) -> Data.Function.Fn0 a
```


#### `on`

``` purescript
on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
```



## Module DOM.XHR

#### `ProgressEvent`

``` purescript
data ProgressEvent :: *
```

An `XMLHttpRequest` object instance.
A `FormData` object instance.
A `ProgressEvent` object instance.

#### `FormData`

``` purescript
data FormData :: *
```


#### `XMLHttpRequest`

``` purescript
data XMLHttpRequest :: *
```



## Module DOM.File

#### `Blob`

``` purescript
data Blob :: *
```

A `File` object instance.
A `FileList` object instance.
A `FileReader` object instance.
A `Blob` object instance.

#### `FileReader`

``` purescript
data FileReader :: *
```


#### `FileList`

``` purescript
data FileList :: *
```


#### `File`

``` purescript
data File :: *
```



## Module DOM

#### `NodeList`

``` purescript
data NodeList :: *
```

Effect type for DOM maniupulation
General type for DOM documents.
General type for DOM nodes.
General type for DOM node lists.

#### `Node`

``` purescript
data Node :: *
```


#### `Document`

``` purescript
data Document :: *
```


#### `DOM`

``` purescript
data DOM :: !
```



## Module Control.Monad.Eff

#### `Pure`

``` purescript
type Pure (a :: *) = forall e. Control.Monad.Eff.Eff e a
```

The `Eff` type constructor is used to represent _native_ effects.

See [Handling Native Effects with the Eff Monad](https://github.com/purescript/purescript/wiki/Handling-Native-Effects-with-the-Eff-Monad) for more details.

The first type parameter is a row of effects which represents the contexts in which a computation can be run, and the second type parameter is the return type.
The `Pure` type synonym represents _pure_ computations, i.e. ones in which all effects have been handled.

The `runPure` function can be used to run pure computations and obtain their result.
Run a pure computation and return its result.

Note: since this function has a rank-2 type, it may cause problems to apply this function using the `$` operator. The recommended approach
is to use parentheses instead.
Loop until a condition becomes `true`.

`untilE b` is an effectful computation which repeatedly runs the effectful computation `b`,
until its return value is `true`.
Loop while a condition is `true`.

`whileE b m` is effectful computation which runs the effectful computation `b`. If its result is
`true`, it runs the effectful computation `m` and loops. If not, the computation ends.
Loop over a consecutive collection of numbers.

`forE lo hi f` runs the computation returned by the function `f` for each of the inputs
between `lo` (inclusive) and `hi` (exclusive).
Loop over an array of values.

`foreach xs f` runs the computation returned by the function `f` for each of the inputs `xs`.

#### `Eff`

``` purescript
data Eff :: # ! -> * -> *
```


#### `foreachE`

``` purescript
foreachE :: forall e a. [a] -> (a -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
```


#### `forE`

``` purescript
forE :: forall e. Number -> Number -> (Number -> Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
```


#### `whileE`

``` purescript
whileE :: forall e a. Control.Monad.Eff.Eff e Boolean -> Control.Monad.Eff.Eff e a -> Control.Monad.Eff.Eff e Prelude.Unit
```


#### `untilE`

``` purescript
untilE :: forall e. Control.Monad.Eff.Eff e Boolean -> Control.Monad.Eff.Eff e Prelude.Unit
```


#### `runPure`

``` purescript
runPure :: forall a. Control.Monad.Eff.Pure a -> a
```



## Module Control.Monad.Eff.Random

#### `Random`

``` purescript
data Random :: !
```

The `Random` effect indicates that an Eff action may access or modify the
JavaScript global random number generator, i.e. `Math.random()`.
Returns a random number between 0 (inclusive) and 1 (exclusive). This is
a direct wrapper around JavaScript's `Math.random()`.
Returns a random number between a minimum value (inclusive) and a maximum
value (exclusive). It is unspecified what happens if `maximum < minimum`.

For example:

* `randomRange 1 2 >>= Debug.Trace.print`

will print a random number between 1 and 2.
Takes a range specified by `low` (the first argument) and `high` (the
second), and returns a random integer uniformly distributed in the closed
interval `[low, high]`. It is unspecified what happens if `low > high`,
or if either of `low` or `high` is not an integer.

For example:

* `randomInt 1 10 >>= Debug.Trace.print`

will print a random integer between 1 and 10.

#### `randomRange`

``` purescript
randomRange :: forall e. Number -> Number -> Control.Monad.Eff.Eff (random :: Control.Monad.Eff.Random.Random | e) Number
```


#### `randomInt`

``` purescript
randomInt :: forall e. Number -> Number -> Control.Monad.Eff.Eff (random :: Control.Monad.Eff.Random.Random | e) Number
```


#### `random`

``` purescript
random :: forall e. Control.Monad.Eff.Eff (random :: Control.Monad.Eff.Random.Random | e) Number
```



## Module Control.Monad.Eff.Ref

#### `RefVal`

``` purescript
data RefVal :: * -> *
```

The effect associated with the use of global mutable variables.
A value of type `RefVal a` represents a mutable reference
which holds a value of type `a`.
Create a new mutable reference containing the specified value.
Read the current value of a mutable reference
Update the value of a mutable reference by applying a function
to the current value.
Update the value of a mutable reference to the specified value.
Update the value of a mutable reference by applying a function
to the current value.

#### `Ref`

``` purescript
data Ref :: !
```


#### `writeRef`

``` purescript
writeRef :: forall s r. Control.Monad.Eff.Ref.RefVal s -> s -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) Prelude.Unit
```


#### `modifyRef`

``` purescript
modifyRef :: forall s r. Control.Monad.Eff.Ref.RefVal s -> (s -> s) -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) Prelude.Unit
```


#### `modifyRef'`

``` purescript
modifyRef' :: forall s b r. Control.Monad.Eff.Ref.RefVal s -> (s -> { newState :: s, retVal :: b }) -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) b
```


#### `readRef`

``` purescript
readRef :: forall s r. Control.Monad.Eff.Ref.RefVal s -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) s
```


#### `newRef`

``` purescript
newRef :: forall s r. s -> Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | r) (Control.Monad.Eff.Ref.RefVal s)
```



## Module Control.Monad.Eff.Ref.Unsafe

#### `unsafeRunRef`

``` purescript
unsafeRunRef :: forall eff a. Control.Monad.Eff.Eff (ref :: Control.Monad.Eff.Ref.Ref | eff) a -> Control.Monad.Eff.Eff eff a
```

This handler function unsafely removes the `Ref` effect from an
effectful action.

This function might be used when it is impossible to prove to the
typechecker that a particular mutable reference does not escape
its scope.


## Module Control.Monad.Eff.Unsafe

#### `unsafeInterleaveEff`

``` purescript
unsafeInterleaveEff :: forall eff1 eff2 a. Control.Monad.Eff.Eff eff1 a -> Control.Monad.Eff.Eff eff2 a
```

Change the type of an effectful computation, allowing it to be run in another context.

Note: use of this function can result in arbitrary side-effects.


## Module Control.Monad.ST

#### `STRef`

``` purescript
data STRef :: * -> * -> *
```

The `ST` effect represents _local mutation_, i.e. mutation which does not "escape" into the surrounding computation.

An `ST` computation is parameterized by a phantom type which is used to restrict the set of reference cells it is allowed to access.

The `runST` function can be used to handle the `ST` effect.
The type `STRef s a` represents a mutable reference holding a value of type `a`, which can be used with the `ST s` effect.
Create a new mutable reference.
Read the current value of a mutable reference.
Modify the value of a mutable reference by applying a function to the current value.
Set the value of a mutable reference.
Run an `ST` computation.

Note: the type of `runST` uses a rank-2 type to constrain the phantom type `s`, such that the computation must not leak any mutable references
to the surrounding computation.

It may cause problems to apply this function using the `$` operator. The recommended approach is to use parentheses instead.
A convenience function which combines `runST` with `runPure`, which can be used when the only required effect is `ST`.

Note: since this function has a rank-2 type, it may cause problems to apply this function using the `$` operator. The recommended approach
is to use parentheses instead.

#### `ST`

``` purescript
data ST :: * -> !
```


#### `pureST`

``` purescript
pureST :: forall a. (forall h r. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a) -> a
```


#### `runST`

``` purescript
runST :: forall a r. (forall h. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a) -> Control.Monad.Eff.Eff r a
```


#### `writeSTRef`

``` purescript
writeSTRef :: forall a h r. Control.Monad.ST.STRef h a -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
```


#### `modifySTRef`

``` purescript
modifySTRef :: forall a h r. Control.Monad.ST.STRef h a -> (a -> a) -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
```


#### `readSTRef`

``` purescript
readSTRef :: forall a h r. Control.Monad.ST.STRef h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) a
```


#### `newSTRef`

``` purescript
newSTRef :: forall a h r. a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Control.Monad.ST.STRef h a)
```



## Module Control.Timer

#### `EffTimer`

``` purescript
type EffTimer (e :: # !) (a :: *) = Control.Monad.Eff.Eff (timer :: Control.Timer.Timer | e) a
```


#### `Milliseconds`

``` purescript
type Milliseconds = Number
```


#### `Interval`

``` purescript
data Interval :: *
```


#### `Timeout`

``` purescript
data Timeout :: *
```


#### `Timer`

``` purescript
data Timer :: !
```


#### `clearInterval`

``` purescript
clearInterval :: forall e. Control.Timer.Interval -> Control.Timer.EffTimer e Prelude.Unit
```


#### `interval`

``` purescript
interval :: forall a e. Control.Timer.Milliseconds -> Control.Timer.EffTimer e a -> Control.Timer.EffTimer e Control.Timer.Interval
```


#### `clearTimeout`

``` purescript
clearTimeout :: forall e. Control.Timer.Timeout -> Control.Timer.EffTimer e Prelude.Unit
```


#### `timeout`

``` purescript
timeout :: forall a e. Control.Timer.Milliseconds -> Control.Timer.EffTimer e a -> Control.Timer.EffTimer e Control.Timer.Timeout
```



## Module Debug.Trace

#### `Trace`

``` purescript
data Trace :: !
```

The `Trace` effect represents those computations which write to the console.
Write a `String` to the console.
Write a value to the console, using its `Show` instance to produce a `String`.

#### `print`

``` purescript
print :: forall a r. (Prelude.Show a) => a -> Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | r) Prelude.Unit
```


#### `trace`

``` purescript
trace :: forall r. String -> Control.Monad.Eff.Eff (trace :: Debug.Trace.Trace | r) Prelude.Unit
```



## Module Mario

#### `Activity`

``` purescript
data Activity
  = Walking 
  | Standing 
  | Jumping 
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


#### `Character`

``` purescript
type Character = { node :: DOM.Node, x :: Number, y :: Number, dx :: Number, dy :: Number, dir :: Mario.Direction }
```


#### `marioLogic`

``` purescript
marioLogic :: forall r. { left :: Boolean, right :: Boolean, jump :: Boolean | r } -> Mario.Character -> Mario.Character
```


#### `jump`

``` purescript
jump :: Boolean -> Mario.Character -> Mario.Character
```


#### `walk`

``` purescript
walk :: Boolean -> Boolean -> Mario.Character -> Mario.Character
```


#### `applyGravity`

``` purescript
applyGravity :: Mario.Character -> Mario.Character
```


#### `velocity`

``` purescript
velocity :: Mario.Character -> Mario.Character
```


#### `currentActivity`

``` purescript
currentActivity :: Mario.Character -> Mario.Activity
```


#### `friction`

``` purescript
friction :: Mario.Character -> Number
```


#### `accel`

``` purescript
accel :: Mario.Character -> Number
```


#### `isAirborne`

``` purescript
isAirborne :: Mario.Character -> Boolean
```


#### `charSpriteDescriptor`

``` purescript
charSpriteDescriptor :: Mario.Character -> Mario.SpriteDescriptor
```


#### `minJumpSpeed`

``` purescript
minJumpSpeed :: Number
```


#### `jumpCoefficient`

``` purescript
jumpCoefficient :: Number
```


#### `airFriction`

``` purescript
airFriction :: Number
```


#### `groundFriction`

``` purescript
groundFriction :: Number
```


#### `airAccel`

``` purescript
airAccel :: Number
```


#### `groundAccel`

``` purescript
groundAccel :: Number
```


#### `maxMoveSpeed`

``` purescript
maxMoveSpeed :: Number
```


#### `gravity`

``` purescript
gravity :: Number
```



## Module Signal

#### `Signal`

``` purescript
data Signal :: * -> *
```


#### `(~)`

``` purescript
(~) :: forall f a b. (Prelude.Apply f) => f (a -> b) -> f a -> f b
```


#### `(~>)`

``` purescript
(~>) :: forall f a b. (Prelude.Functor f) => f a -> (a -> b) -> f b
```


#### `(<~)`

``` purescript
(<~) :: forall f a b. (Prelude.Functor f) => (a -> b) -> f a -> f b
```


#### `keepIf`

``` purescript
keepIf :: forall a. (a -> Boolean) -> a -> Signal.Signal a -> Signal.Signal a
```


#### `unwrap`

``` purescript
unwrap :: forall a e. Signal.Signal (Control.Monad.Eff.Eff e a) -> Control.Monad.Eff.Eff e (Signal.Signal a)
```


#### `runSignal`

``` purescript
runSignal :: forall e. Signal.Signal (Control.Monad.Eff.Eff e Prelude.Unit) -> Control.Monad.Eff.Eff e Prelude.Unit
```


#### `zip`

``` purescript
zip :: forall a b c. (a -> b -> c) -> Signal.Signal a -> Signal.Signal b -> Signal.Signal c
```


#### `distinct'`

``` purescript
distinct' :: forall a. Signal.Signal a -> Signal.Signal a
```


#### `distinct`

``` purescript
distinct :: forall a. (Prelude.Eq a) => Signal.Signal a -> Signal.Signal a
```


#### `sampleOn`

``` purescript
sampleOn :: forall a b. Signal.Signal a -> Signal.Signal b -> Signal.Signal b
```


#### `foldp`

``` purescript
foldp :: forall a b. (a -> b -> b) -> b -> Signal.Signal a -> Signal.Signal b
```


#### `merge`

``` purescript
merge :: forall a. Signal.Signal a -> Signal.Signal a -> Signal.Signal a
```


#### `applySig`

``` purescript
applySig :: forall a b. Signal.Signal (a -> b) -> Signal.Signal a -> Signal.Signal b
```


#### `map`

``` purescript
map :: forall a b. (a -> b) -> Signal.Signal a -> Signal.Signal b
```


#### `constant`

``` purescript
constant :: forall a. a -> Signal.Signal a
```



## Module Signal.Channel

#### `Chan`

``` purescript
data Chan :: !
```


#### `Channel`

``` purescript
data Channel :: * -> *
```


#### `subscribe`

``` purescript
subscribe :: forall a. Signal.Channel.Channel a -> Signal.Signal a
```


#### `send`

``` purescript
send :: forall a e. Signal.Channel.Channel a -> a -> Control.Monad.Eff.Eff (chan :: Signal.Channel.Chan | e) Prelude.Unit
```


#### `channel`

``` purescript
channel :: forall a e. a -> Control.Monad.Eff.Eff (chan :: Signal.Channel.Chan | e) (Signal.Channel.Channel a)
```



## Module Signal.Time

#### `Time`

``` purescript
type Time = Number
```

Returns the number of milliseconds since an arbitrary, but constant, time in the past.

#### `second`

``` purescript
second :: Signal.Time.Time
```


#### `now`

``` purescript
now :: forall e. Control.Monad.Eff.Eff (timer :: Control.Timer.Timer | e) Signal.Time.Time
```


#### `millisecond`

``` purescript
millisecond :: Signal.Time.Time
```


#### `every`

``` purescript
every :: Signal.Time.Time -> Signal.Signal Signal.Time.Time
```



## Module Signal.DOM

#### `Touch`

``` purescript
type Touch = { id :: String, screenX :: Number, screenY :: Number, clientX :: Number, clientY :: Number, pageX :: Number, pageY :: Number, radiusX :: Number, radiusY :: Number, rotationAngle :: Number, force :: Number }
```


#### `CoordinatePair`

``` purescript
type CoordinatePair = { x :: Number, y :: Number }
```


#### `mousePos`

``` purescript
mousePos :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Signal.DOM.CoordinatePair)
```


#### `tap`

``` purescript
tap :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Boolean)
```


#### `touch`

``` purescript
touch :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal [Signal.DOM.Touch])
```


#### `mouseButton`

``` purescript
mouseButton :: forall e. Number -> Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Boolean)
```


#### `keyPressed`

``` purescript
keyPressed :: forall e. Number -> Control.Monad.Eff.Eff (dom :: DOM.DOM | e) (Signal.Signal Boolean)
```


#### `animationFrame`

``` purescript
animationFrame :: forall e. Control.Monad.Eff.Eff (dom :: DOM.DOM, timer :: Control.Timer.Timer | e) (Signal.Signal Signal.Time.Time)
```



## Module Control.Monad

#### `filterM`

``` purescript
filterM :: forall a m. (Prelude.Monad m) => (a -> m Boolean) -> [a] -> m [a]
```

Perform a monadic action when a condition is true.
Perform a monadic action unless a condition is true.
Perform a monadic action `n` times collecting all of the results.
Perform a fold using a monadic step function.
Filter where the predicate returns a monadic `Boolean`.

For example: 

```purescript
powerSet :: forall a. [a] -> [[a]]
powerSet = filterM (const [true, false])
```

#### `unless`

``` purescript
unless :: forall m. (Prelude.Monad m) => Boolean -> m Prelude.Unit -> m Prelude.Unit
```


#### `when`

``` purescript
when :: forall m. (Prelude.Monad m) => Boolean -> m Prelude.Unit -> m Prelude.Unit
```


#### `foldM`

``` purescript
foldM :: forall m a b. (Prelude.Monad m) => (a -> b -> m a) -> a -> [b] -> m a
```


#### `replicateM`

``` purescript
replicateM :: forall m a. (Prelude.Monad m) => Number -> m a -> m [a]
```



## Module Control.Lazy

#### `Lazy2`

``` purescript
class Lazy2 l where
  defer2 :: forall a b. (Prelude.Unit -> l a b) -> l a b
```

A version of `Lazy` for type constructors of two type arguments.
A version of `Lazy` for type constructors of one type argument.
The `Lazy` class represents types which allow evaluation of values
to be _deferred_.

Usually, this means that a type contains a function arrow which can
be used to delay evaluation.
The `Lazy` class represents types which allow evaluation of values
to be _deferred_.

Usually, this means that a type contains a function arrow which can
be used to delay evaluation.
A version of `Lazy` for type constructors of one type argument.
A version of `Lazy` for type constructors of two type arguments.
A version of `Lazy` for type constructors of two type arguments.
A version of `fix` for type constructors of two type arguments.
A version of `Lazy` for type constructors of one type argument.
A version of `fix` for type constructors of one type argument.
The `Lazy` class represents types which allow evaluation of values
to be _deferred_.

Usually, this means that a type contains a function arrow which can
be used to delay evaluation.
`fix` defines a value as the fixed point of a function.

The `Lazy` instance allows us to generate the result lazily.

#### `Lazy1`

``` purescript
class Lazy1 l where
  defer1 :: forall a. (Prelude.Unit -> l a) -> l a
```


#### `Lazy`

``` purescript
class Lazy l where
  defer :: (Prelude.Unit -> l) -> l
```


#### `fix2`

``` purescript
fix2 :: forall l a b. (Control.Lazy.Lazy2 l) => (l a b -> l a b) -> l a b
```


#### `fix1`

``` purescript
fix1 :: forall l a. (Control.Lazy.Lazy1 l) => (l a -> l a) -> l a
```


#### `fix`

``` purescript
fix :: forall l a. (Control.Lazy.Lazy l) => (l -> l) -> l
```



## Module Control.Functor

#### `($>)`

``` purescript
($>) :: forall f a b. (Prelude.Functor f) => f a -> b -> f b
```

Ignore the return value of a computation, using the specified return value instead.
A version of `(<$)` with its arguments flipped.

#### `(<$)`

``` purescript
(<$) :: forall f a b. (Prelude.Functor f) => a -> f b -> f a
```



## Module Control.Extend

#### `Extend`

``` purescript
class (Prelude.Functor w) <= Extend w where
  (<<=) :: forall b a. (w a -> b) -> w a -> w b
```

The `Extend` class defines the extension operator `(<<=)`
which extends a local context-dependent computation to
a global computation.

`Extend` is the dual of `Bind`, and `(<<=)` is the dual of 
`(>>=)`.

Laws:

- Associativity: `extend f <<< extend g = extend (f <<< extend g)`
Backwards co-Kleisli composition.
Forwards co-Kleisli composition.
A version of `(<<=)` with its arguments flipped.
An alias for `(<<=)`.
Duplicate a comonadic context.

`duplicate` is dual to `Control.Bind.join`.

#### `duplicate`

``` purescript
duplicate :: forall a w. (Control.Extend.Extend w) => w a -> w (w a)
```


#### `extend`

``` purescript
extend :: forall b a w. (Control.Extend.Extend w) => (w a -> b) -> w a -> w b
```


#### `(=<=)`

``` purescript
(=<=) :: forall b a w c. (Control.Extend.Extend w) => (w b -> c) -> (w a -> b) -> w a -> c
```


#### `(=>=)`

``` purescript
(=>=) :: forall b a w c. (Control.Extend.Extend w) => (w a -> b) -> (w b -> c) -> w a -> c
```


#### `(=>>)`

``` purescript
(=>>) :: forall b a w. (Control.Extend.Extend w) => w a -> (w a -> b) -> w b
```



## Module Control.Comonad

#### `Comonad`

``` purescript
class (Control.Extend.Extend w) <= Comonad w where
  extract :: forall a. w a -> a
```

`Comonad` extends the `Extend` class with the `extract` function
which extracts a value, discarding the comonadic context.

`Comonad` is the dual of `Monad`, and `extract` is the dual of 
`pure` or `return`.

Laws:

- Left Identity: `extract <<= xs = xs`
- Right Identity: `extract (f <<= xs) = f xs`
`Comonad` extends the `Extend` class with the `extract` function
which extracts a value, discarding the comonadic context.

`Comonad` is the dual of `Monad`, and `extract` is the dual of 
`pure` or `return`.

Laws:

- Left Identity: `extract <<= xs = xs`
- Right Identity: `extract (f <<= xs) = f xs`
`Comonad` extends the `Extend` class with the `extract` function
which extracts a value, discarding the comonadic context.

`Comonad` is the dual of `Monad`, and `extract` is the dual of 
`pure` or `return`.

Laws:

- Left Identity: `extract <<= xs = xs`
- Right Identity: `extract (f <<= xs) = f xs`


## Module Control.Bind

#### `ifM`

``` purescript
ifM :: forall a m. (Prelude.Bind m) => m Boolean -> m a -> m a -> m a
```

Forwards Kleisli composition.

For example:

```purescript
import Data.Array (head, tail)

third = tail >=> tail >=> head
```
A version of `(>>=)` with its arguments flipped.
Backwards Kleisli composition.
Collapse two applications of a monadic type constructor into one.
Execute a monadic action if a condition holds. 

For example:

```purescript
main = ifM ((< 0.5) <$> random)
         (trace "Heads")
         (trace "Tails")
```

#### `join`

``` purescript
join :: forall a m. (Prelude.Bind m) => m (m a) -> m a
```


#### `(<=<)`

``` purescript
(<=<) :: forall a b c m. (Prelude.Bind m) => (b -> m c) -> (a -> m b) -> a -> m c
```


#### `(>=>)`

``` purescript
(>=>) :: forall a b c m. (Prelude.Bind m) => (a -> m b) -> (b -> m c) -> a -> m c
```


#### `(=<<)`

``` purescript
(=<<) :: forall a b m. (Prelude.Bind m) => (a -> m b) -> m a -> m b
```



## Module Control.Apply

#### `lift5`

``` purescript
lift5 :: forall a b c d e f g. (Prelude.Apply f) => (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
```

Combine two effectful actions, keeping only the result of the first.
Combine two effectful actions, keeping only the result of the second.
Lift a function of five arguments to a function which accepts and returns
values wrapped with the type constructor `f`.
Lift a function of four arguments to a function which accepts and returns
values wrapped with the type constructor `f`.
Lift a function of three arguments to a function which accepts and returns
values wrapped with the type constructor `f`.
Lift a function of two arguments to a function which accepts and returns
values wrapped with the type constructor `f`.

#### `lift4`

``` purescript
lift4 :: forall a b c d e f. (Prelude.Apply f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
```


#### `lift3`

``` purescript
lift3 :: forall a b c d f. (Prelude.Apply f) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
```


#### `lift2`

``` purescript
lift2 :: forall a b c f. (Prelude.Apply f) => (a -> b -> c) -> f a -> f b -> f c
```


#### `(*>)`

``` purescript
(*>) :: forall a b f. (Prelude.Apply f) => f a -> f b -> f b
```


#### `(<*)`

``` purescript
(<*) :: forall a b f. (Prelude.Apply f) => f a -> f b -> f a
```



## Module Control.Alt

#### `Alt`

``` purescript
class (Prelude.Functor f) <= Alt f where
  (<|>) :: forall a. f a -> f a -> f a
```

The `Alt` type class identifies an associative operation on a type
constructor.  It is similar to `Semigroup`, except that it applies to
types of kind `* -> *`, like `Array` or `List`, rather than concrete types
`String` or `Number`.

`Alt` instances are required to satisfy the following laws:

- Associativity: `(x <|> y) <|> z == x <|> (y <|> z)`
- Distributivity: `f <$> (x <|> y) == (f <$> x) <|> (f <$> y)`

For example, the `Array` (`[]`) type is an instance of `Alt`, where
`(<|>)` is defined to be concatenation.


## Module Control.Plus

#### `Plus`

``` purescript
class (Control.Alt.Alt f) <= Plus f where
  empty :: forall a. f a
```

The `Plus` type class extends the `Alt` type class with a value that
should be the left and right identity for `(<|>)`.

It is similar to `Monoid`, except that it applies to types of
kind `* -> *`, like `Array` or `List`, rather than concrete types like
`String` or `Number`.

`Plus` instances should satisfy the following laws:

- Left identity: `empty <|> x == x`
- Right identity: `x <|> empty == x`
- Annihilation: `f <$> empty == empty`
The `Plus` type class extends the `Alt` type class with a value that
should be the left and right identity for `(<|>)`.

It is similar to `Monoid`, except that it applies to types of
kind `* -> *`, like `Array` or `List`, rather than concrete types like
`String` or `Number`.

`Plus` instances should satisfy the following laws:

- Left identity: `empty <|> x == x`
- Right identity: `x <|> empty == x`
- Annihilation: `f <$> empty == empty`
The `Plus` type class extends the `Alt` type class with a value that
should be the left and right identity for `(<|>)`.

It is similar to `Monoid`, except that it applies to types of
kind `* -> *`, like `Array` or `List`, rather than concrete types like
`String` or `Number`.

`Plus` instances should satisfy the following laws:

- Left identity: `empty <|> x == x`
- Right identity: `x <|> empty == x`
- Annihilation: `f <$> empty == empty`


## Module Control.Alternative

#### `Alternative`

``` purescript
class (Prelude.Applicative f, Control.Plus.Plus f) <= Alternative f where
```

The `Alternative` type class has no members of its own; it just specifies
that the type constructor has both `Applicative` and `Plus` instances.

Types which have `Alternative` instances should also satisfy the following
laws:

- Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
- Annihilation: `empty <*> f = empty`
The `Alternative` type class has no members of its own; it just specifies
that the type constructor has both `Applicative` and `Plus` instances.

Types which have `Alternative` instances should also satisfy the following
laws:

- Distributivity: `(f <|> g) <*> x == (f <*> x) <|> (g <*> x)`
- Annihilation: `empty <*> f = empty`

#### `many`

``` purescript
many :: forall f a. (Control.Alternative.Alternative f, Control.Lazy.Lazy1 f) => f a -> f [a]
```


#### `some`

``` purescript
some :: forall f a. (Control.Alternative.Alternative f, Control.Lazy.Lazy1 f) => f a -> f [a]
```



## Module Control.MonadPlus

#### `MonadPlus`

``` purescript
class (Prelude.Monad m, Control.Alternative.Alternative m) <= MonadPlus m where
```

The `MonadPlus` type class has no members of its own; it just specifies
that the type has both `Monad` and `Alternative` instances.

Types which have `MonadPlus` instances should also satisfy the following
laws:

- Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
- Annihilation: `empty >>= f = empty`
The `MonadPlus` type class has no members of its own; it just specifies
that the type has both `Monad` and `Alternative` instances.

Types which have `MonadPlus` instances should also satisfy the following
laws:

- Distributivity: `(x <|> y) >>= f == (x >>= f) <|> (y >>= f)`
- Annihilation: `empty >>= f = empty`
Fail using `Plus` if a condition does not hold, or
succeed using `Monad` if it does.

For example:

```purescript
import Data.Array

factors :: Number -> [Number]
factors n = do
  a <- 1 .. n
  b <- 1 .. a
  guard $ a * b == n
  return a
```

#### `guard`

``` purescript
guard :: forall m. (Control.MonadPlus.MonadPlus m) => Boolean -> m Prelude.Unit
```



## Module Data.Either

#### `Either`

``` purescript
data Either (a :: *) (b :: *)
  = Left a
  | Right b
```

The `Either` type is used to represent a choice between two types of value.

A common use case for `Either` is error handling, where `Left` is used to
carry an error value and `Right` is used to carry a success value.
The `Functor` instance allows functions to transform the contents of a
`Right` with the `<$>` operator:

``` purescript
f <$> Right x == Right (f x)
```

`Left` values are untouched:

``` purescript
f <$> Left y == Left y
```
The `Apply` instance allows functions contained within a `Right` to
transform a value contained within a `Right` using the `(<*>)` operator:

``` purescript
Right f <*> Right x == Right (f x)
```

`Left` values are left untouched:

``` purescript
Left f <*> Right x == Left x
Right f <*> Left y == Left y
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
pure function to take `Either`-typed arguments so `f :: a -> b -> c`
becomes `f :: Either l a -> Either l b -> Either l c`:

``` purescript
f <$> Right x <*> Right y == Right (f x y)
```

The `Left`-preserving behaviour of both operators means the result of
an expression like the above but where any one of the values is `Left`
means the whole result becomes `Left` also, taking the first `Left` value
found:

``` purescript
f <$> Left x <*> Right y == Left x
f <$> Right x <*> Left y == Left y
f <$> Left x <*> Left y == Left x
```
The `Applicative` instance enables lifting of values into `Either` with the
`pure` or `return` function (`return` is an alias for `pure`):

``` purescript
pure x :: Either _ _ == Right x
return x :: Either _ _ == Right x
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
`pure` can be used to pass a mixture of `Either` and non-`Either` typed
values to a function that does not usually expect them, by using `pure`
for any value that is not already `Either` typed:

``` purescript
f <$> Right x <*> pure y == Right (f x y)
```

Even though `pure = Right` it is recommended to use `pure` in situations
like this as it allows the choice of `Applicative` to be changed later
without having to go through and replace `Right` with a new constructor.
The `Alt` instance allows for a choice to be made between two `Either`
values with the `<|>` operator, where the first `Right` encountered
is taken.

``` purescript
Right x <|> Right y == Right x
Left x <|> Right y == Right y
Left x <|> Left y == Left y
```
The `Bind` instance allows sequencing of `Either` values and functions that
return an `Either` by using the `>>=` operator:

``` purescript
Left x >>= f = Left x
Right x >>= f = f x
```
The `Monad` instance guarantees that there are both `Applicative` and
`Bind` instances for `Either`. This also enables the `do` syntactic sugar:

``` purescript
do
  x' <- x
  y' <- y
  pure (f x' y')
```

Which is equivalent to:

``` purescript
x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
```
The `Extend` instance allows sequencing of `Either` values and functions
that accept an `Either` and return a non-`Either` result using the
`<<=` operator.

``` purescript
f <<= Left x = Left x
f <<= Right x = Right (f x)
```
The `Show` instance allows `Either` values to be rendered as a string with
`show` whenever there is an `Show` instance for both type the `Either` can
contain.
The `Eq` instance allows `Either` values to be checked for equality with
`==` and inequality with `/=` whenever there is an `Eq` instance for both
types the `Either` can contain.
The `Ord` instance allows `Either` values to be compared with
`compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
both types the `Either` can contain.

Any `Left` value is considered to be less than a `Right` value.
The `Show` instance allows `Either` values to be rendered as a string with
`show` whenever there is an `Show` instance for both type the `Either` can
contain.
The `Functor` instance allows functions to transform the contents of a
`Right` with the `<$>` operator:

``` purescript
f <$> Right x == Right (f x)
```

`Left` values are untouched:

``` purescript
f <$> Left y == Left y
```
The `Extend` instance allows sequencing of `Either` values and functions
that accept an `Either` and return a non-`Either` result using the
`<<=` operator.

``` purescript
f <<= Left x = Left x
f <<= Right x = Right (f x)
```
The `Eq` instance allows `Either` values to be checked for equality with
`==` and inequality with `/=` whenever there is an `Eq` instance for both
types the `Either` can contain.
The `Ord` instance allows `Either` values to be compared with
`compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
both types the `Either` can contain.

Any `Left` value is considered to be less than a `Right` value.
Takes two functions and an `Either` value, if the value is a `Left` the
inner value is applied to the first function, if the value is a `Right`
the inner value is applied to the second function.

``` purescript
either f g (Left x) == f x
either f g (Right y) == g y
```
Returns `true` when the `Either` value was constructed with `Left`.
Returns `true` when the `Either` value was constructed with `Right`.
The `Apply` instance allows functions contained within a `Right` to
transform a value contained within a `Right` using the `(<*>)` operator:

``` purescript
Right f <*> Right x == Right (f x)
```

`Left` values are left untouched:

``` purescript
Left f <*> Right x == Left x
Right f <*> Left y == Left y
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
pure function to take `Either`-typed arguments so `f :: a -> b -> c`
becomes `f :: Either l a -> Either l b -> Either l c`:

``` purescript
f <$> Right x <*> Right y == Right (f x y)
```

The `Left`-preserving behaviour of both operators means the result of
an expression like the above but where any one of the values is `Left`
means the whole result becomes `Left` also, taking the first `Left` value
found:

``` purescript
f <$> Left x <*> Right y == Left x
f <$> Right x <*> Left y == Left y
f <$> Left x <*> Left y == Left x
```
The `Bind` instance allows sequencing of `Either` values and functions that
return an `Either` by using the `>>=` operator:

``` purescript
Left x >>= f = Left x
Right x >>= f = f x
```
The `Applicative` instance enables lifting of values into `Either` with the
`pure` or `return` function (`return` is an alias for `pure`):

``` purescript
pure x :: Either _ _ == Right x
return x :: Either _ _ == Right x
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
`pure` can be used to pass a mixture of `Either` and non-`Either` typed
values to a function that does not usually expect them, by using `pure`
for any value that is not already `Either` typed:

``` purescript
f <$> Right x <*> pure y == Right (f x y)
```

Even though `pure = Right` it is recommended to use `pure` in situations
like this as it allows the choice of `Applicative` to be changed later
without having to go through and replace `Right` with a new constructor.
The `Monad` instance guarantees that there are both `Applicative` and
`Bind` instances for `Either`. This also enables the `do` syntactic sugar:

``` purescript
do
  x' <- x
  y' <- y
  pure (f x' y')
```

Which is equivalent to:

``` purescript
x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
```
The `Alt` instance allows for a choice to be made between two `Either`
values with the `<|>` operator, where the first `Right` encountered
is taken.

``` purescript
Right x <|> Right y == Right x
Left x <|> Right y == Right y
Left x <|> Left y == Left y
```

#### `isRight`

``` purescript
isRight :: forall a b. Data.Either.Either a b -> Boolean
```


#### `isLeft`

``` purescript
isLeft :: forall a b. Data.Either.Either a b -> Boolean
```


#### `either`

``` purescript
either :: forall a b c. (a -> c) -> (b -> c) -> Data.Either.Either a b -> c
```



## Module Data.Either.Nested

#### `Either10`

``` purescript
type Either10 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (g :: *) (h :: *) (i :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either9 a b c d e f g h i) z
```

#### `Either9`

``` purescript
type Either9 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (g :: *) (h :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either8 a b c d e f g h) z
```


#### `Either8`

``` purescript
type Either8 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (g :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either7 a b c d e f g) z
```


#### `Either7`

``` purescript
type Either7 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either6 a b c d e f) z
```


#### `Either6`

``` purescript
type Either6 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either5 a b c d e) z
```


#### `Either5`

``` purescript
type Either5 (a :: *) (b :: *) (c :: *) (d :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either4 a b c d) z
```


#### `Either4`

``` purescript
type Either4 (a :: *) (b :: *) (c :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either3 a b c) z
```


#### `Either3`

``` purescript
type Either3 (a :: *) (b :: *) (z :: *) = Data.Either.Either (Data.Either.Nested.Either2 a b) z
```


#### `Either2`

``` purescript
type Either2 (a :: *) (z :: *) = Data.Either.Either a z
```


#### `either10`

``` purescript
either10 :: forall a b c d e f g h i j z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> (j -> z) -> Data.Either.Nested.Either10 a b c d e f g h i j -> z
```


#### `either9`

``` purescript
either9 :: forall a b c d e f g h i z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> (i -> z) -> Data.Either.Nested.Either9 a b c d e f g h i -> z
```


#### `either8`

``` purescript
either8 :: forall a b c d e f g h z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> (h -> z) -> Data.Either.Nested.Either8 a b c d e f g h -> z
```


#### `either7`

``` purescript
either7 :: forall a b c d e f g z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> (g -> z) -> Data.Either.Nested.Either7 a b c d e f g -> z
```


#### `either6`

``` purescript
either6 :: forall a b c d e f z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> (f -> z) -> Data.Either.Nested.Either6 a b c d e f -> z
```


#### `either5`

``` purescript
either5 :: forall a b c d e z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> (e -> z) -> Data.Either.Nested.Either5 a b c d e -> z
```


#### `either4`

``` purescript
either4 :: forall a b c d z. (a -> z) -> (b -> z) -> (c -> z) -> (d -> z) -> Data.Either.Nested.Either4 a b c d -> z
```


#### `either3`

``` purescript
either3 :: forall a b c z. (a -> z) -> (b -> z) -> (c -> z) -> Data.Either.Nested.Either3 a b c -> z
```


#### `either2`

``` purescript
either2 :: forall a b z. (a -> z) -> (b -> z) -> Data.Either.Nested.Either2 a b -> z
```


#### `either10of10`

``` purescript
either10of10 :: forall a b c d e f g h i j. j -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either9of10`

``` purescript
either9of10 :: forall a b c d e f g h i j. i -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either8of10`

``` purescript
either8of10 :: forall a b c d e f g h i j. h -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either7of10`

``` purescript
either7of10 :: forall a b c d e f g h i j. g -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either6of10`

``` purescript
either6of10 :: forall a b c d e f g h i j. f -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either5of10`

``` purescript
either5of10 :: forall a b c d e f g h i j. e -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either4of10`

``` purescript
either4of10 :: forall a b c d e f g h i j. d -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either3of10`

``` purescript
either3of10 :: forall a b c d e f g h i j. c -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either2of10`

``` purescript
either2of10 :: forall a b c d e f g h i j. b -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either1of10`

``` purescript
either1of10 :: forall a b c d e f g h i j. a -> Data.Either.Nested.Either10 a b c d e f g h i j
```


#### `either9of9`

``` purescript
either9of9 :: forall a b c d e f g h i. i -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either8of9`

``` purescript
either8of9 :: forall a b c d e f g h i. h -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either7of9`

``` purescript
either7of9 :: forall a b c d e f g h i. g -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either6of9`

``` purescript
either6of9 :: forall a b c d e f g h i. f -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either5of9`

``` purescript
either5of9 :: forall a b c d e f g h i. e -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either4of9`

``` purescript
either4of9 :: forall a b c d e f g h i. d -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either3of9`

``` purescript
either3of9 :: forall a b c d e f g h i. c -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either2of9`

``` purescript
either2of9 :: forall a b c d e f g h i. b -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either1of9`

``` purescript
either1of9 :: forall a b c d e f g h i. a -> Data.Either.Nested.Either9 a b c d e f g h i
```


#### `either8of8`

``` purescript
either8of8 :: forall a b c d e f g h. h -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either7of8`

``` purescript
either7of8 :: forall a b c d e f g h. g -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either6of8`

``` purescript
either6of8 :: forall a b c d e f g h. f -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either5of8`

``` purescript
either5of8 :: forall a b c d e f g h. e -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either4of8`

``` purescript
either4of8 :: forall a b c d e f g h. d -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either3of8`

``` purescript
either3of8 :: forall a b c d e f g h. c -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either2of8`

``` purescript
either2of8 :: forall a b c d e f g h. b -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either1of8`

``` purescript
either1of8 :: forall a b c d e f g h. a -> Data.Either.Nested.Either8 a b c d e f g h
```


#### `either7of7`

``` purescript
either7of7 :: forall a b c d e f g. g -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either6of7`

``` purescript
either6of7 :: forall a b c d e f g. f -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either5of7`

``` purescript
either5of7 :: forall a b c d e f g. e -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either4of7`

``` purescript
either4of7 :: forall a b c d e f g. d -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either3of7`

``` purescript
either3of7 :: forall a b c d e f g. c -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either2of7`

``` purescript
either2of7 :: forall a b c d e f g. b -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either1of7`

``` purescript
either1of7 :: forall a b c d e f g. a -> Data.Either.Nested.Either7 a b c d e f g
```


#### `either6of6`

``` purescript
either6of6 :: forall a b c d e f. f -> Data.Either.Nested.Either6 a b c d e f
```


#### `either5of6`

``` purescript
either5of6 :: forall a b c d e f. e -> Data.Either.Nested.Either6 a b c d e f
```


#### `either4of6`

``` purescript
either4of6 :: forall a b c d e f. d -> Data.Either.Nested.Either6 a b c d e f
```


#### `either3of6`

``` purescript
either3of6 :: forall a b c d e f. c -> Data.Either.Nested.Either6 a b c d e f
```


#### `either2of6`

``` purescript
either2of6 :: forall a b c d e f. b -> Data.Either.Nested.Either6 a b c d e f
```


#### `either1of6`

``` purescript
either1of6 :: forall a b c d e f. a -> Data.Either.Nested.Either6 a b c d e f
```


#### `either5of5`

``` purescript
either5of5 :: forall a b c d e. e -> Data.Either.Nested.Either5 a b c d e
```


#### `either4of5`

``` purescript
either4of5 :: forall a b c d e. d -> Data.Either.Nested.Either5 a b c d e
```


#### `either3of5`

``` purescript
either3of5 :: forall a b c d e. c -> Data.Either.Nested.Either5 a b c d e
```


#### `either2of5`

``` purescript
either2of5 :: forall a b c d e. b -> Data.Either.Nested.Either5 a b c d e
```


#### `either1of5`

``` purescript
either1of5 :: forall a b c d e. a -> Data.Either.Nested.Either5 a b c d e
```


#### `either4of4`

``` purescript
either4of4 :: forall a b c d. d -> Data.Either.Nested.Either4 a b c d
```


#### `either3of4`

``` purescript
either3of4 :: forall a b c d. c -> Data.Either.Nested.Either4 a b c d
```


#### `either2of4`

``` purescript
either2of4 :: forall a b c d. b -> Data.Either.Nested.Either4 a b c d
```


#### `either1of4`

``` purescript
either1of4 :: forall a b c d. a -> Data.Either.Nested.Either4 a b c d
```


#### `either3of3`

``` purescript
either3of3 :: forall a b c. c -> Data.Either.Nested.Either3 a b c
```


#### `either2of3`

``` purescript
either2of3 :: forall a b c. b -> Data.Either.Nested.Either3 a b c
```


#### `either1of3`

``` purescript
either1of3 :: forall a b c. a -> Data.Either.Nested.Either3 a b c
```


#### `either2of2`

``` purescript
either2of2 :: forall a b. b -> Data.Either.Nested.Either2 a b
```


#### `either1of2`

``` purescript
either1of2 :: forall a b. a -> Data.Either.Nested.Either2 a b
```



## Module Data.Either.Unsafe

#### `fromRight`

``` purescript
fromRight :: forall a b. Data.Either.Either a b -> b
```

A partial function that extracts the value from the `Right` data constructor.
Passing a `Left` to `fromRight` will throw an error at runtime.
A partial function that extracts the value from the `Left` data constructor.
Passing a `Right` to `fromLeft` will throw an error at runtime.

#### `fromLeft`

``` purescript
fromLeft :: forall a b. Data.Either.Either a b -> a
```



## Module Data.Maybe

#### `Maybe`

``` purescript
data Maybe (a :: *)
  = Nothing 
  | Just a
```

The `Maybe` type is used to represent optional values and can be seen as
something like a type-safe `null`, where `Nothing` is `null` and `Just x`
is the non-null value `x`.
The `Functor` instance allows functions to transform the contents of a
`Just` with the `<$>` operator:

``` purescript
f <$> Just x == Just (f x)
```

`Nothing` values are left untouched:

``` purescript
f <$> Nothing == Nothing
```
The `Apply` instance allows functions contained within a `Just` to
transform a value contained within a `Just` using the `(<*>)` operator:

``` purescript
Just f <*> Just x == Just (f x)
```

`Nothing` values are left untouched:

``` purescript
Just f <*> Nothing == Nothing
Nothing <*> Just x == Nothing
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
pure function to take `Maybe`-typed arguments so `f :: a -> b -> c`
becomes `f :: Maybe a -> Maybe b -> Maybe c`:

``` purescript
f <$> Just x <*> Just y == Just (f x y)
```

The `Nothing`-preserving behaviour of both operators means the result of
an expression like the above but where any one of the values is `Nothing`
means the whole result becomes `Nothing` also:

``` purescript
f <$> Nothing <*> Just y == Nothing
f <$> Just x <*> Nothing == Nothing
f <$> Nothing <*> Nothing == Nothing
```
The `Applicative` instance enables lifting of values into `Maybe` with the
`pure` or `return` function (`return` is an alias for `pure`):

``` purescript
pure x :: Maybe _ == Just x
return x :: Maybe _ == Just x
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
`pure` can be used to pass a mixture of `Maybe` and non-`Maybe` typed
values to a function that does not usually expect them, by using `pure`
for any value that is not already `Maybe` typed:

``` purescript
f <$> Just x <*> pure y == Just (f x y)
```

Even though `pure = Just` it is recommended to use `pure` in situations
like this as it allows the choice of `Applicative` to be changed later
without having to go through and replace `Just` with a new constructor.
The `Alt` instance allows for a choice to be made between two `Maybe`
values with the `<|>` operator, where the first `Just` encountered
is taken.

``` purescript
Just x <|> Just y == Just x
Nothing <|> Just y == Just y
Nothing <|> Nothing == Nothing
```
The `Plus` instance provides a default `Maybe` value:

``` purescript
empty :: Maybe _ == Nothing
```
The `Alternative` instance guarantees that there are both `Applicative` and
`Plus` instances for `Maybe`.
The `Bind` instance allows sequencing of `Maybe` values and functions that
return a `Maybe` by using the `>>=` operator:

``` purescript
Just x >>= f = f x
Nothing >>= f = Nothing
```
The `Monad` instance guarantees that there are both `Applicative` and
`Bind` instances for `Maybe`. This also enables the `do` syntactic sugar:

``` purescript
do
  x' <- x
  y' <- y
  pure (f x' y')
```

Which is equivalent to:

``` purescript
x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
```
The `MonadPlus` instance guarantees that there are both `Monad` and
`Alternative` instances for `Maybe`.
The `Extend` instance allows sequencing of `Maybe` values and functions
that accept a `Maybe a` and return a non-`Maybe` result using the
`<<=` operator.

``` purescript
f <<= Nothing = Nothing
f <<= Just x = Just (f x)
```
The `Semigroup` instance enables use of the operator `<>` on `Maybe` values
whenever there is a `Semigroup` instance for the type the `Maybe` contains.
The exact behaviour of `<>` depends on the "inner" `Semigroup` instance,
but generally captures the notion of appending or combining things.

``` purescript
Just x <> Just y = Just (x <> y)
Just x <> Nothing = Just x
Nothing <> Just y = Just y
Nothing <> Nothing = Nothing
```
The `Show` instance allows `Maybe` values to be rendered as a string with
`show` whenever there is an `Show` instance for the type the `Maybe`
contains.
The `Eq` instance allows `Maybe` values to be checked for equality with
`==` and inequality with `/=` whenever there is an `Eq` instance for the
type the `Maybe` contains.
The `Ord` instance allows `Maybe` values to be compared with
`compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
the type the `Maybe` contains.

`Nothing` is considered to be less than any `Just` value.
The `Show` instance allows `Maybe` values to be rendered as a string with
`show` whenever there is an `Show` instance for the type the `Maybe`
contains.
The `Semigroup` instance enables use of the operator `<>` on `Maybe` values
whenever there is a `Semigroup` instance for the type the `Maybe` contains.
The exact behaviour of `<>` depends on the "inner" `Semigroup` instance,
but generally captures the notion of appending or combining things.

``` purescript
Just x <> Just y = Just (x <> y)
Just x <> Nothing = Just x
Nothing <> Just y = Just y
Nothing <> Nothing = Nothing
```
Takes a default value, a function, and a `Maybe` value. If the `Maybe`
value is `Nothing` the default value is returned, otherwise the function
is applied to the value inside the `Just` and the result is returned.

``` purescript
maybe x f Nothing == x
maybe x f (Just y) == f y
```
Returns `true` when the `Maybe` value is `Nothing`.
Returns `true` when the `Maybe` value was constructed with `Just`.
The `Functor` instance allows functions to transform the contents of a
`Just` with the `<$>` operator:

``` purescript
f <$> Just x == Just (f x)
```

`Nothing` values are left untouched:

``` purescript
f <$> Nothing == Nothing
```
Takes a default value, and a `Maybe` value. If the `Maybe` value is
`Nothing` the default value is returned, otherwise the value inside the
`Just` is returned.

``` purescript
fromMaybe x Nothing == x
fromMaybe x (Just y) == y
```
The `Extend` instance allows sequencing of `Maybe` values and functions
that accept a `Maybe a` and return a non-`Maybe` result using the
`<<=` operator.

``` purescript
f <<= Nothing = Nothing
f <<= Just x = Just (f x)
```
The `Eq` instance allows `Maybe` values to be checked for equality with
`==` and inequality with `/=` whenever there is an `Eq` instance for the
type the `Maybe` contains.
The `Ord` instance allows `Maybe` values to be compared with
`compare`, `>`, `>=`, `<` and `<=` whenever there is an `Ord` instance for
the type the `Maybe` contains.

`Nothing` is considered to be less than any `Just` value.
The `Apply` instance allows functions contained within a `Just` to
transform a value contained within a `Just` using the `(<*>)` operator:

``` purescript
Just f <*> Just x == Just (f x)
```

`Nothing` values are left untouched:

``` purescript
Just f <*> Nothing == Nothing
Nothing <*> Just x == Nothing
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` can be used transform a
pure function to take `Maybe`-typed arguments so `f :: a -> b -> c`
becomes `f :: Maybe a -> Maybe b -> Maybe c`:

``` purescript
f <$> Just x <*> Just y == Just (f x y)
```

The `Nothing`-preserving behaviour of both operators means the result of
an expression like the above but where any one of the values is `Nothing`
means the whole result becomes `Nothing` also:

``` purescript
f <$> Nothing <*> Just y == Nothing
f <$> Just x <*> Nothing == Nothing
f <$> Nothing <*> Nothing == Nothing
```
The `Bind` instance allows sequencing of `Maybe` values and functions that
return a `Maybe` by using the `>>=` operator:

``` purescript
Just x >>= f = f x
Nothing >>= f = Nothing
```
The `Applicative` instance enables lifting of values into `Maybe` with the
`pure` or `return` function (`return` is an alias for `pure`):

``` purescript
pure x :: Maybe _ == Just x
return x :: Maybe _ == Just x
```

Combining `Functor`'s `<$>` with `Apply`'s `<*>` and `Applicative`'s
`pure` can be used to pass a mixture of `Maybe` and non-`Maybe` typed
values to a function that does not usually expect them, by using `pure`
for any value that is not already `Maybe` typed:

``` purescript
f <$> Just x <*> pure y == Just (f x y)
```

Even though `pure = Just` it is recommended to use `pure` in situations
like this as it allows the choice of `Applicative` to be changed later
without having to go through and replace `Just` with a new constructor.
The `Monad` instance guarantees that there are both `Applicative` and
`Bind` instances for `Maybe`. This also enables the `do` syntactic sugar:

``` purescript
do
  x' <- x
  y' <- y
  pure (f x' y')
```

Which is equivalent to:

``` purescript
x >>= (\x' -> y >>= (\y' -> pure (f x' y')))
```
The `Alt` instance allows for a choice to be made between two `Maybe`
values with the `<|>` operator, where the first `Just` encountered
is taken.

``` purescript
Just x <|> Just y == Just x
Nothing <|> Just y == Just y
Nothing <|> Nothing == Nothing
```
The `Plus` instance provides a default `Maybe` value:

``` purescript
empty :: Maybe _ == Nothing
```
The `Alternative` instance guarantees that there are both `Applicative` and
`Plus` instances for `Maybe`.
The `MonadPlus` instance guarantees that there are both `Monad` and
`Alternative` instances for `Maybe`.

#### `isNothing`

``` purescript
isNothing :: forall a. Data.Maybe.Maybe a -> Boolean
```


#### `isJust`

``` purescript
isJust :: forall a. Data.Maybe.Maybe a -> Boolean
```


#### `fromMaybe`

``` purescript
fromMaybe :: forall a. a -> Data.Maybe.Maybe a -> a
```


#### `maybe`

``` purescript
maybe :: forall a b. b -> (a -> b) -> Data.Maybe.Maybe a -> b
```



## Module Data.Array

#### `replicate`

``` purescript
replicate :: forall a. Number -> a -> [a]
```

Append an element to the end of an array, creating a new array.
Get the number of elements in an array
Find the first index for which a predicate holds,
or `-1` if no such element exists
Find the last index for which a predicate holds,
or `-1` if no such element exists
Concatenate two arrays, creating a new array
Flatten an array of arrays, creating a new array
Reverse an array, creating a copy
Drop a number of elements from the start of an array, creating a new array.
Create a copy of a subarray
Insert an element at the specified index, creating a new array.
Delete the element at the specified index, creating a new array.
Change the element at the specified index, creating a new array.
Apply a function to each element in an array, and flatten the results
into a single, new array.
Apply a function to each element in an array, creating a new array.
Filter an array, keeping the elements which satisfy a predicate function,
creating a new array.
Create an array containing a range of numbers, including both endpoints.
Apply a function to pairs of elements at the same index in two arrays,
collecting the results in a new array.

If one array is longer, elements will be discarded from the longer array.

For example

```purescript
zipWith (*) [1, 2, 3] [4, 5, 6, 7] == [4, 10, 18]
```
Create an array with repeated instances of a value.
An infix synonym for `range`.
This operator provides a safe way to read a value at a particular index from an array.

This function returns `Nothing` if the index is out-of-bounds.

`Data.Array.Unsafe` provides the `unsafeIndex` function, which is an unsafe version of
this function without bounds checking.
Keep only a number of elements from the start of an array, creating a new array.
Get all but the first element of an array, creating a new array, or `Nothing` if the array is empty

Running time: `O(n)` where `n` is the length of the array
Split an array into two parts:

1. the longest initial subarray for which all element satisfy the specified predicate
2. the remaining elements

For example,

```purescript
span (\n -> n % 2 == 1) [1,3,2,4,5] == { init: [1,3], rest: [2,4,5] }
```
Calculate the longest initial subarray for which all element satisfy the specified predicate,
creating a new array.
Sort the elements of an array in increasing order, where elements are compared using
the specified partial ordering, creating a new array.
Sort the elements of an array in increasing order, creating a new array.
Create an array of one element
Test whether an array is empty.
Remove the duplicates from an array, where element equality is determined by the
specified equivalence relation, creating a new array.
Remove the duplicates from an array, creating a new array.
Apply a function to the element at the specified index, creating a new array.
Apply a function to each element in an array, keeping only the results which
contain a value, creating a new array.
Get the last element in an array, or `Nothing` if the array is empty

Running time: `O(1)`.
Calculate the intersection of two arrays, using the specified equivalence relation
to compare elements, creating a new array.
Calculate the intersection of two arrays, creating a new array.
Get all but the last element of an array, creating a new array, or `Nothing` if the array is empty.

Running time: `O(n)` where `n` is the length of the array
Get the first element in an array, or `Nothing` if the array is empty

Running time: `O(1)`.
Group equal, consecutive elements of an array into arrays, using the specified
equivalence relation to detemine equality.
Group equal, consecutive elements of an array into arrays.

For example,

```purescript
group [1,1,2,2,1] == [[1,1],[2,2],[1]]
```
Sort and group the elements of an array into arrays.

For example,

```purescript
group [1,1,2,2,1] == [[1,1,1],[2,2]]
```
Find the index of the last element equal to the specified element,
or `-1` if no such element exists
Find the index of the first element equal to the specified element,
or `-1` if no such element exists
Remove the longest initial subarray for which all element satisfy the specified predicate,
creating a new array.
Delete the first element of an array which matches the specified value, under the
equivalence relation provided in the first argument, creating a new array.
Delete the first element of an array which is equal to the specified value,
creating a new array.
Delete the first occurrence of each element in the second array from the first array,
creating a new array.
Filter an array of optional values, keeping only the elements which contain
a value, creating a new array.

#### `takeWhile`

``` purescript
takeWhile :: forall a. (a -> Boolean) -> [a] -> [a]
```


#### `dropWhile`

``` purescript
dropWhile :: forall a. (a -> Boolean) -> [a] -> [a]
```


#### `span`

``` purescript
span :: forall a. (a -> Boolean) -> [a] -> { init :: [a], rest :: [a] }
```


#### `groupBy`

``` purescript
groupBy :: forall a. (a -> a -> Boolean) -> [a] -> [[a]]
```


#### `group'`

``` purescript
group' :: forall a. (Prelude.Ord a) => [a] -> [[a]]
```


#### `group`

``` purescript
group :: forall a. (Prelude.Eq a) => [a] -> [[a]]
```


#### `sortBy`

``` purescript
sortBy :: forall a. (a -> a -> Prelude.Ordering) -> [a] -> [a]
```


#### `sort`

``` purescript
sort :: forall a. (Prelude.Ord a) => [a] -> [a]
```


#### `nubBy`

``` purescript
nubBy :: forall a. (a -> a -> Boolean) -> [a] -> [a]
```


#### `nub`

``` purescript
nub :: forall a. (Prelude.Eq a) => [a] -> [a]
```


#### `zipWith`

``` purescript
zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
```


#### `range`

``` purescript
range :: Number -> Number -> [Number]
```


#### `filter`

``` purescript
filter :: forall a. (a -> Boolean) -> [a] -> [a]
```


#### `concatMap`

``` purescript
concatMap :: forall a b. (a -> [b]) -> [a] -> [b]
```


#### `intersect`

``` purescript
intersect :: forall a. (Prelude.Eq a) => [a] -> [a] -> [a]
```


#### `intersectBy`

``` purescript
intersectBy :: forall a. (a -> a -> Boolean) -> [a] -> [a] -> [a]
```


#### `(\\)`

``` purescript
(\\) :: forall a. (Prelude.Eq a) => [a] -> [a] -> [a]
```


#### `delete`

``` purescript
delete :: forall a. (Prelude.Eq a) => a -> [a] -> [a]
```


#### `deleteBy`

``` purescript
deleteBy :: forall a. (a -> a -> Boolean) -> a -> [a] -> [a]
```


#### `modifyAt`

``` purescript
modifyAt :: forall a. Number -> (a -> a) -> [a] -> [a]
```


#### `updateAt`

``` purescript
updateAt :: forall a. Number -> a -> [a] -> [a]
```


#### `deleteAt`

``` purescript
deleteAt :: forall a. Number -> Number -> [a] -> [a]
```


#### `insertAt`

``` purescript
insertAt :: forall a. Number -> a -> [a] -> [a]
```


#### `take`

``` purescript
take :: forall a. Number -> [a] -> [a]
```


#### `drop`

``` purescript
drop :: forall a. Number -> [a] -> [a]
```


#### `reverse`

``` purescript
reverse :: forall a. [a] -> [a]
```


#### `concat`

``` purescript
concat :: forall a. [[a]] -> [a]
```


#### `append`

``` purescript
append :: forall a. [a] -> [a] -> [a]
```


#### `elemLastIndex`

``` purescript
elemLastIndex :: forall a. (Prelude.Eq a) => a -> [a] -> Number
```


#### `elemIndex`

``` purescript
elemIndex :: forall a. (Prelude.Eq a) => a -> [a] -> Number
```


#### `findLastIndex`

``` purescript
findLastIndex :: forall a. (a -> Boolean) -> [a] -> Number
```


#### `findIndex`

``` purescript
findIndex :: forall a. (a -> Boolean) -> [a] -> Number
```


#### `length`

``` purescript
length :: forall a. [a] -> Number
```


#### `catMaybes`

``` purescript
catMaybes :: forall a. [Data.Maybe.Maybe a] -> [a]
```


#### `mapMaybe`

``` purescript
mapMaybe :: forall a b. (a -> Data.Maybe.Maybe b) -> [a] -> [b]
```


#### `map`

``` purescript
map :: forall a b. (a -> b) -> [a] -> [b]
```


#### `null`

``` purescript
null :: forall a. [a] -> Boolean
```


#### `init`

``` purescript
init :: forall a. [a] -> Data.Maybe.Maybe [a]
```


#### `tail`

``` purescript
tail :: forall a. [a] -> Data.Maybe.Maybe [a]
```


#### `last`

``` purescript
last :: forall a. [a] -> Data.Maybe.Maybe a
```


#### `head`

``` purescript
head :: forall a. [a] -> Data.Maybe.Maybe a
```


#### `singleton`

``` purescript
singleton :: forall a. a -> [a]
```


#### `snoc`

``` purescript
snoc :: forall a. [a] -> a -> [a]
```


#### `(..)`

``` purescript
(..) :: Number -> Number -> [Number]
```


#### `(!!)`

``` purescript
(!!) :: forall a. [a] -> Number -> Data.Maybe.Maybe a
```



## Module Data.Array.ST

#### `Assoc`

``` purescript
type Assoc (a :: *) = { value :: a, index :: Number }
```

A reference to a mutable array.

The first type parameter represents the memory region which the array belongs to.
The second type parameter defines the type of elements of the mutable array.

The runtime representation of a value of type `STArray h a` is the same as that of `[a]`,
except that mutation is allowed.
An element and its index
Freeze a mutable array, creating an immutable array. Use this function as you would use
`runST` to freeze a mutable reference.

The rank-2 type prevents the reference from escaping the scope of `runSTArray`.
Create an empty mutable array.
Create an immutable copy of a mutable array, where each element
is labelled with its index in the original array.
Create a mutable copy of an immutable array.
Remove and/or insert elements from/into a mutable array at the specified index.
Append the values in an immutable array to the end of a mutable array.
Append an element to the end of a mutable array.
Change the value at the specified index in a mutable array.
Read the value at the specified index in a mutable array.
Create an immutable copy of a mutable array.

#### `STArray`

``` purescript
data STArray :: * -> * -> *
```


#### `toAssocArray`

``` purescript
toAssocArray :: forall a h r. Data.Array.ST.STArray h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) [Data.Array.ST.Assoc a]
```


#### `thaw`

``` purescript
thaw :: forall a h r. [a] -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Array.ST.STArray h a)
```


#### `freeze`

``` purescript
freeze :: forall a h r. Data.Array.ST.STArray h a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) [a]
```


#### `spliceSTArray`

``` purescript
spliceSTArray :: forall a h r. Data.Array.ST.STArray h a -> Number -> Number -> [a] -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) [a]
```


#### `pushAllSTArray`

``` purescript
pushAllSTArray :: forall a h r. Data.Array.ST.STArray h a -> [a] -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) Number
```


#### `pushSTArray`

``` purescript
pushSTArray :: forall a h r. Data.Array.ST.STArray h a -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) Number
```


#### `pokeSTArray`

``` purescript
pokeSTArray :: forall a h r. Data.Array.ST.STArray h a -> Number -> a -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) Boolean
```


#### `peekSTArray`

``` purescript
peekSTArray :: forall a h r. Data.Array.ST.STArray h a -> Number -> Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Maybe.Maybe a)
```


#### `emptySTArray`

``` purescript
emptySTArray :: forall a h r. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Array.ST.STArray h a)
```


#### `runSTArray`

``` purescript
runSTArray :: forall a r. (forall h. Control.Monad.Eff.Eff (st :: Control.Monad.ST.ST h | r) (Data.Array.ST.STArray h a)) -> Control.Monad.Eff.Eff r [a]
```



## Module Data.Maybe.Unsafe

#### `fromJust`

``` purescript
fromJust :: forall a. Data.Maybe.Maybe a -> a
```

A partial function that extracts the value from the `Just` data
constructor. Passing `Nothing` to `fromJust` will throw an error at
runtime.


## Module Data.Array.Unsafe

#### `init`

``` purescript
init :: forall a. [a] -> [a]
```

Get all but the first element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.
Get the last element of a non-empty array.

Running time: `O(1)`.
Get all but the last element of a non-empty array.

Running time: `O(n)`, where `n` is the length of the array.
Get the first element of a non-empty array.

Running time: `O(1)`.

#### `last`

``` purescript
last :: forall a. [a] -> a
```


#### `tail`

``` purescript
tail :: forall a. [a] -> [a]
```


#### `head`

``` purescript
head :: forall a. [a] -> a
```



## Module Data.Monoid

#### `Monoid`

``` purescript
class (Prelude.Semigroup m) <= Monoid m where
  mempty :: m
```



## Module Data.Monoid.Additive

#### `Additive`

``` purescript
newtype Additive (a :: *)
  = Additive a
```

Monoid and semigroup for semirings under addition.

``` purescript
Additive x <> Additive y == Additive (x + y)
mempty :: Additive _ == Additive zero
```

#### `runAdditive`

``` purescript
runAdditive :: forall a. Data.Monoid.Additive.Additive a -> a
```



## Module Data.Monoid.All

#### `All`

``` purescript
newtype All
  = All Prim.Boolean
```

Boolean monoid and semigroup under conjunction.

``` purescript
All x <> All y == All (x && y)
mempty :: All == All true
```

#### `runAll`

``` purescript
runAll :: Data.Monoid.All.All -> Boolean
```



## Module Data.Monoid.Any

#### `Any`

``` purescript
newtype Any
  = Any Prim.Boolean
```

Boolean monoid and semigroup under disjunction.

``` purescript
Any x <> Any y == Any (x || y)
mempty :: Any == Any false
```

#### `runAny`

``` purescript
runAny :: Data.Monoid.Any.Any -> Boolean
```



## Module Data.Monoid.Dual

#### `Dual`

``` purescript
newtype Dual (a :: *)
  = Dual a
```

The dual of a monoid.

``` purescript
Dual x <> Dual y == Dual (y <> x)
mempty :: Dual _ == Dual mempty
```

#### `runDual`

``` purescript
runDual :: forall a. Data.Monoid.Dual.Dual a -> a
```



## Module Data.Monoid.Endo

#### `Endo`

``` purescript
newtype Endo (a :: *)
  = Endo (a -> a)
```

Monoid of endomorphisms under composition.

Composes of functions of type `a -> a`:
``` purescript
Endo f <> Endo g == Endo (f <<< g)
mempty :: Endo _ == Endo id
```

#### `runEndo`

``` purescript
runEndo :: forall a. Data.Monoid.Endo.Endo a -> a -> a
```



## Module Data.Monoid.Multiplicative

#### `Multiplicative`

``` purescript
newtype Multiplicative (a :: *)
  = Multiplicative a
```

Monoid and semigroup for semirings under multiplication.

``` purescript
Multiplicative x <> Multiplicative y == Multiplicative (x * y)
mempty :: Multiplicative _ == Multiplicative one
```

#### `runMultiplicative`

``` purescript
runMultiplicative :: forall a. Data.Monoid.Multiplicative.Multiplicative a -> a
```



## Module Data.Tuple

#### `Tuple`

``` purescript
data Tuple (a :: *) (b :: *)
  = Tuple a b
```

A simple product type for wrapping a pair of component values.
Allows `Tuple`s to be rendered as a string with `show` whenever there are
`Show` instances for both component types.
Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
there are `Eq` instances for both component types.
Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
whenever there are `Ord` instances for both component types. To obtain
the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
`snd`s are `compare`d.
The `Semigroup` instance enables use of the associative operator `<>` on
`Tuple`s whenever there are `Semigroup` instances for the component
types. The `<>` operator is applied pairwise, so:
```purescript
(Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
```
The `Functor` instance allows functions to transform the contents of a
`Tuple` with the `<$>` operator, applying the function to the second
component, so:
```purescript
f <$> (Tuple x y) = Tuple x (f y)
````
The `Functor` instance allows functions to transform the contents of a
`Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
for the `fst` component, so:
```purescript
(Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
```
Rakes two lists and returns a list of corresponding pairs.
If one input list is short, excess elements of the longer list are discarded.
Transforms a list of pairs into a list of first components and a list of
second components.
Turn a function of two arguments into a function that expects a tuple.
Exchange the first and second components of a tuple.
Returns the second component of a tuple.
Allows `Tuple`s to be rendered as a string with `show` whenever there are
`Show` instances for both component types.
The `Semigroup` instance enables use of the associative operator `<>` on
`Tuple`s whenever there are `Semigroup` instances for the component
types. The `<>` operator is applied pairwise, so:
```purescript
(Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
```
The `Functor` instance allows functions to transform the contents of a
`Tuple` with the `<$>` operator, applying the function to the second
component, so:
```purescript
f <$> (Tuple x y) = Tuple x (f y)
````
Returns the first component of a tuple.
Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
there are `Eq` instances for both component types.
Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
whenever there are `Ord` instances for both component types. To obtain
the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
`snd`s are `compare`d.
Turn a function that expects a tuple into a function of two arguments.
The `Functor` instance allows functions to transform the contents of a
`Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
for the `fst` component, so:
```purescript
(Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
```

#### `swap`

``` purescript
swap :: forall a b. Data.Tuple.Tuple a b -> Data.Tuple.Tuple b a
```


#### `unzip`

``` purescript
unzip :: forall a b. [Data.Tuple.Tuple a b] -> Data.Tuple.Tuple [a] [b]
```


#### `zip`

``` purescript
zip :: forall a b. [a] -> [b] -> [Data.Tuple.Tuple a b]
```


#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Data.Tuple.Tuple a b -> c
```


#### `curry`

``` purescript
curry :: forall a b c. (Data.Tuple.Tuple a b -> c) -> a -> b -> c
```


#### `snd`

``` purescript
snd :: forall a b. Data.Tuple.Tuple a b -> b
```


#### `fst`

``` purescript
fst :: forall a b. Data.Tuple.Tuple a b -> a
```



## Module Data.Tuple.Nested

#### `Tuple10`

``` purescript
type Tuple10 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (g :: *) (h :: *) (i :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple9 a b c d e f g h i) z
```

Shorthand for constructing n-tuples as nested pairs.
`a /\ b /\ c /\ d` becomes `Tuple (Tuple (Tuple a b) c ) d`
Given a function of 9 arguments, return a function that accepts a 9-tuple.
Given a function of 8 arguments, return a function that accepts a 8-tuple.
Given a function of 7 arguments, return a function that accepts a 7-tuple.
Given a function of 6 arguments, return a function that accepts a 6-tuple.
Given a function of 5 arguments, return a function that accepts a 5-tuple.
Given a function of 4 arguments, return a function that accepts a 4-tuple.
Given a function of 3 arguments, return a function that accepts a 3-tuple.
Given a function of 2 arguments, return a function that accepts a 2-tuple.
Given a function of 10 arguments, return a function that accepts a 10-tuple.
Given 9 values, creates a nested 9-tuple.
Given 8 values, creates a nested 8-tuple.
Given 7 values, creates a nested 7-tuple.
Given 6 values, creates a nested 6-tuple.
Given 5 values, creates a nested 5-tuple.
Given 4 values, creates a nested 4-tuple.
Given 3 values, creates a nested 3-tuple.
Given 2 values, creates a nested 2-tuple.
Given 10 values, creates a nested 10-tuple.
Given a function that accepts a 9-tuple, return a function of 9 arguments.
Given a function that accepts a 8-tuple, return a function of 8 arguments.
Given a function that accepts a 7-tuple, return a function of 7 arguments.
Given a function that accepts a 6-tuple, return a function of 6 arguments.
Given a function that accepts a 5-tuple, return a function of 5 arguments.
Given a function that accepts a 4-tuple, return a function of 4 arguments.
Given a function that accepts a 3-tuple, return a function of 3 arguments.
Given a function that accepts a 2-tuple, return a function of 2 arguments.
Given a function that accepts a 10-tuple, return a function of 10 arguments.

#### `Tuple9`

``` purescript
type Tuple9 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (g :: *) (h :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple8 a b c d e f g h) z
```


#### `Tuple8`

``` purescript
type Tuple8 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (g :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple7 a b c d e f g) z
```


#### `Tuple7`

``` purescript
type Tuple7 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (f :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple6 a b c d e f) z
```


#### `Tuple6`

``` purescript
type Tuple6 (a :: *) (b :: *) (c :: *) (d :: *) (e :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple5 a b c d e) z
```


#### `Tuple5`

``` purescript
type Tuple5 (a :: *) (b :: *) (c :: *) (d :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple4 a b c d) z
```


#### `Tuple4`

``` purescript
type Tuple4 (a :: *) (b :: *) (c :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple3 a b c) z
```


#### `Tuple3`

``` purescript
type Tuple3 (a :: *) (b :: *) (z :: *) = Data.Tuple.Tuple (Data.Tuple.Nested.Tuple2 a b) z
```


#### `Tuple2`

``` purescript
type Tuple2 (a :: *) (z :: *) = Data.Tuple.Tuple a z
```


#### `(/\)`

``` purescript
(/\) :: forall a b. a -> b -> Data.Tuple.Tuple a b
```


#### `curry10`

``` purescript
curry10 :: forall a b c d e f g h i j z. (Data.Tuple.Nested.Tuple10 a b c d e f g h i j -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z
```


#### `uncurry10`

``` purescript
uncurry10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Data.Tuple.Nested.Tuple10 a b c d e f g h i j -> z
```


#### `curry9`

``` purescript
curry9 :: forall a b c d e f g h i z. (Data.Tuple.Nested.Tuple9 a b c d e f g h i -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> z
```


#### `uncurry9`

``` purescript
uncurry9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Data.Tuple.Nested.Tuple9 a b c d e f g h i -> z
```


#### `curry8`

``` purescript
curry8 :: forall a b c d e f g h z. (Data.Tuple.Nested.Tuple8 a b c d e f g h -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> z
```


#### `uncurry8`

``` purescript
uncurry8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Data.Tuple.Nested.Tuple8 a b c d e f g h -> z
```


#### `curry7`

``` purescript
curry7 :: forall a b c d e f g z. (Data.Tuple.Nested.Tuple7 a b c d e f g -> z) -> a -> b -> c -> d -> e -> f -> g -> z
```


#### `uncurry7`

``` purescript
uncurry7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Data.Tuple.Nested.Tuple7 a b c d e f g -> z
```


#### `curry6`

``` purescript
curry6 :: forall a b c d e f z. (Data.Tuple.Nested.Tuple6 a b c d e f -> z) -> a -> b -> c -> d -> e -> f -> z
```


#### `uncurry6`

``` purescript
uncurry6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Data.Tuple.Nested.Tuple6 a b c d e f -> z
```


#### `curry5`

``` purescript
curry5 :: forall a b c d e z. (Data.Tuple.Nested.Tuple5 a b c d e -> z) -> a -> b -> c -> d -> e -> z
```


#### `uncurry5`

``` purescript
uncurry5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Data.Tuple.Nested.Tuple5 a b c d e -> z
```


#### `curry4`

``` purescript
curry4 :: forall a b c d z. (Data.Tuple.Nested.Tuple4 a b c d -> z) -> a -> b -> c -> d -> z
```


#### `uncurry4`

``` purescript
uncurry4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Data.Tuple.Nested.Tuple4 a b c d -> z
```


#### `curry3`

``` purescript
curry3 :: forall a b c z. (Data.Tuple.Nested.Tuple3 a b c -> z) -> a -> b -> c -> z
```


#### `uncurry3`

``` purescript
uncurry3 :: forall a b c z. (a -> b -> c -> z) -> Data.Tuple.Nested.Tuple3 a b c -> z
```


#### `curry2`

``` purescript
curry2 :: forall a b z. (Data.Tuple.Nested.Tuple2 a b -> z) -> a -> b -> z
```


#### `uncurry2`

``` purescript
uncurry2 :: forall a b z. (a -> b -> z) -> Data.Tuple.Nested.Tuple2 a b -> z
```


#### `tuple10`

``` purescript
tuple10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Data.Tuple.Nested.Tuple10 a b c d e f g h i j
```


#### `tuple9`

``` purescript
tuple9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Data.Tuple.Nested.Tuple9 a b c d e f g h i
```


#### `tuple8`

``` purescript
tuple8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Data.Tuple.Nested.Tuple8 a b c d e f g h
```


#### `tuple7`

``` purescript
tuple7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Data.Tuple.Nested.Tuple7 a b c d e f g
```


#### `tuple6`

``` purescript
tuple6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Data.Tuple.Nested.Tuple6 a b c d e f
```


#### `tuple5`

``` purescript
tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Data.Tuple.Nested.Tuple5 a b c d e
```


#### `tuple4`

``` purescript
tuple4 :: forall a b c d. a -> b -> c -> d -> Data.Tuple.Nested.Tuple4 a b c d
```


#### `tuple3`

``` purescript
tuple3 :: forall a b c. a -> b -> c -> Data.Tuple.Nested.Tuple3 a b c
```


#### `tuple2`

``` purescript
tuple2 :: forall a b. a -> b -> Data.Tuple.Nested.Tuple2 a b
```



## Module Data.Monoid.First

#### `First`

``` purescript
newtype First (a :: *)
  = First (Data.Maybe.Maybe a)
```

Monoid returning the first (left-most) non-Nothing value.

``` purescript
First (Just x) <> First (Just y) == First (Just x)
First Nothing <> First (Just y) == First (Just x)
First Nothing <> Nothing == First Nothing
mempty :: First _ == First Nothing
```

#### `runFirst`

``` purescript
runFirst :: forall a. Data.Monoid.First.First a -> Data.Maybe.Maybe a
```



## Module Data.Monoid.Last

#### `Last`

``` purescript
newtype Last (a :: *)
  = Last (Data.Maybe.Maybe a)
```

Monoid returning the last (right-most) non-Nothing value.

``` purescript
Last (Just x) <> Last (Just y) == Last (Just y)
Last (Just x) <> Nothing == Last (Just x)
Last Nothing <> Nothing == Last Nothing
mempty :: Last _ == Last Nothing
```

#### `runLast`

``` purescript
runLast :: forall a. Data.Monoid.Last.Last a -> Data.Maybe.Maybe a
```



## Module Data.Foldable

#### `Foldable`

``` purescript
class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. (Data.Monoid.Monoid m) => (a -> m) -> f a -> m
```

`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`
`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`
`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`
Traverse a data structure, performing some effects encoded by an
`Applicative` functor at each value, ignoring the final result.

For example:

```purescript
traverse_ print [1, 2, 3]
```
A version of `traverse_` with its arguments flipped.

This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for_ [1, 2, 3] \n -> do
  print n
  trace "squared is"
  print (n * n)
```
Perform all of the effects in some data structure in the order
given by the `Foldable` instance, ignoring the final result.

For example:

```purescript
sequence_ [ trace "Hello, ", trace " world!" ]
```
`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`
Fold a data structure, accumulating values in some `Monoid`,
combining adjacent elements using the specified separator. 
Fold a data structure, accumulating values in some `Monoid`.
Test whether any `Boolean` value in a data structure is `true`.
Find the product of the numeric values in a data structure.
Find the sum of the numeric values in a data structure.
`Foldable` represents data structures which can be _folded_.

- `foldr` folds a structure from the right
- `foldl` folds a structure from the left
- `foldMap` folds a structure by accumulating values in a `Monoid`
Lookup a value in a data structure of `Tuple`s, generalizing association lists.
Fold a data structure, accumulating values in some `Monoid`.
Try to find an element in a data structure which satisfies a predicate.
Test whether a predicate holds for any element in a data structure.
Test whether a value is an element of a data structure.
Test whether a value is not an element of a data structure.
Test whether all `Boolean` values in a data structure are `true`.
Test whether a predicate holds for all elements in a data structure.

#### `foldlArray`

``` purescript
foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b
```


#### `foldrArray`

``` purescript
foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b
```


#### `lookup`

``` purescript
lookup :: forall a b f. (Prelude.Eq a, Data.Foldable.Foldable f) => a -> f (Data.Tuple.Tuple a b) -> Data.Maybe.Maybe b
```


#### `find`

``` purescript
find :: forall a f. (Data.Foldable.Foldable f) => (a -> Boolean) -> f a -> Data.Maybe.Maybe a
```


#### `notElem`

``` purescript
notElem :: forall a f. (Prelude.Eq a, Data.Foldable.Foldable f) => a -> f a -> Boolean
```


#### `elem`

``` purescript
elem :: forall a f. (Prelude.Eq a, Data.Foldable.Foldable f) => a -> f a -> Boolean
```


#### `product`

``` purescript
product :: forall f. (Data.Foldable.Foldable f) => f Number -> Number
```


#### `sum`

``` purescript
sum :: forall f. (Data.Foldable.Foldable f) => f Number -> Number
```


#### `all`

``` purescript
all :: forall a f. (Data.Foldable.Foldable f) => (a -> Boolean) -> f a -> Boolean
```


#### `any`

``` purescript
any :: forall a f. (Data.Foldable.Foldable f) => (a -> Boolean) -> f a -> Boolean
```


#### `or`

``` purescript
or :: forall f. (Data.Foldable.Foldable f) => f Boolean -> Boolean
```


#### `and`

``` purescript
and :: forall f. (Data.Foldable.Foldable f) => f Boolean -> Boolean
```


#### `intercalate`

``` purescript
intercalate :: forall f m. (Data.Foldable.Foldable f, Data.Monoid.Monoid m) => m -> f m -> m
```


#### `mconcat`

``` purescript
mconcat :: forall f m. (Data.Foldable.Foldable f, Data.Monoid.Monoid m) => f m -> m
```


#### `sequence_`

``` purescript
sequence_ :: forall a f m. (Prelude.Applicative m, Data.Foldable.Foldable f) => f (m a) -> m Prelude.Unit
```


#### `for_`

``` purescript
for_ :: forall a b f m. (Prelude.Applicative m, Data.Foldable.Foldable f) => f a -> (a -> m b) -> m Prelude.Unit
```


#### `traverse_`

``` purescript
traverse_ :: forall a b f m. (Prelude.Applicative m, Data.Foldable.Foldable f) => (a -> m b) -> f a -> m Prelude.Unit
```


#### `fold`

``` purescript
fold :: forall f m. (Data.Foldable.Foldable f, Data.Monoid.Monoid m) => f m -> m
```



## Module Data.Traversable

#### `Traversable`

``` purescript
class (Prelude.Functor t, Data.Foldable.Foldable t) <= Traversable t where
  traverse :: forall a b m. (Prelude.Applicative m) => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. (Prelude.Applicative m) => t (m a) -> m (t a)
```

`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id` 

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`
`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id` 

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`
`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id` 

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`
`Traversable` represents data structures which can be _traversed_,
accumulating results and effects in some `Applicative` functor.

- `traverse` runs an action for every element in a data structure,
  and accumulates the results.
- `sequence` runs the actions _contained_ in a data structure,
  and accumulates the results.

The `traverse` and `sequence` functions should be compatible in the
following sense:

- `traverse f xs = sequence (f <$> xs)`
- `sequence = traverse id` 

`Traversable` instances should also be compatible with the corresponding
`Foldable` instances, in the following sense:

- `foldMap f = runConst <<< traverse (Const <<< f)`
A generalization of `zipWith` which accumulates results in some `Applicative`
functor.
A version of `traverse` with its arguments flipped.


This can be useful when running an action written using do notation
for every element in a data structure:

For example:

```purescript
for [1, 2, 3] \n -> do
  print n
  return (n * n)
```
Fold a data structure from the right, keeping all intermediate results
instead of only the final result.

Unlike `scanr`, `mapAccumR` allows the type of accumulator to differ
from the element type of the final data structure.
Fold a data structure from the right, keeping all intermediate results
instead of only the final result.
Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

Unlike `scanl`, `mapAccumL` allows the type of accumulator to differ
from the element type of the final data structure.
Fold a data structure from the left, keeping all intermediate results
instead of only the final result.

#### `mapAccumR`

``` purescript
mapAccumR :: forall a b s f. (Data.Traversable.Traversable f) => (s -> a -> Data.Tuple.Tuple s b) -> s -> f a -> Data.Tuple.Tuple s (f b)
```


#### `mapAccumL`

``` purescript
mapAccumL :: forall a b s f. (Data.Traversable.Traversable f) => (s -> a -> Data.Tuple.Tuple s b) -> s -> f a -> Data.Tuple.Tuple s (f b)
```


#### `scanr`

``` purescript
scanr :: forall a b f. (Data.Traversable.Traversable f) => (a -> b -> b) -> b -> f a -> f b
```


#### `scanl`

``` purescript
scanl :: forall a b f. (Data.Traversable.Traversable f) => (b -> a -> b) -> b -> f a -> f b
```


#### `zipWithA`

``` purescript
zipWithA :: forall m a b c. (Prelude.Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
```


#### `for`

``` purescript
for :: forall a b m t. (Prelude.Applicative m, Data.Traversable.Traversable t) => t a -> (a -> m b) -> m (t b)
```



## Module Main

#### `GameState`

``` purescript
type GameState = { mario :: Mario.Character }
```

#### `main`

``` purescript
main :: forall t9378. Control.Monad.Eff.Eff (timer :: Control.Timer.Timer | t9378) Prelude.Unit
```


#### `render`

``` purescript
render :: forall t9342. Control.Monad.Eff.Eff (dom :: DOM.DOM | t9342) Main.GameState -> Control.Monad.Eff.Eff (dom :: DOM.DOM | t9342) Prelude.Unit
```


#### `gameLogic`

``` purescript
gameLogic :: forall r t9350. { left :: Boolean, right :: Boolean, jump :: Boolean | r } -> Control.Monad.Eff.Eff t9350 Main.GameState -> Control.Monad.Eff.Eff t9350 Main.GameState
```


#### `initialState`

``` purescript
initialState :: DOM.Node -> Main.GameState
```


#### `getMarioNode`

``` purescript
getMarioNode :: forall eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) DOM.Node
```


#### `onDOMContentLoaded`

``` purescript
onDOMContentLoaded :: forall a eff. Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) a -> Control.Monad.Eff.Eff eff Prelude.Unit
```


#### `updateSprite`

``` purescript
updateSprite :: forall eff t9311. Mario.Character -> Control.Monad.Eff.Eff (dom :: DOM.DOM | t9311) Prelude.Unit
```


#### `updateSpriteP`

``` purescript
updateSpriteP :: forall eff. DOM.Node -> Mario.SpriteDescriptor -> Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) Prelude.Unit
```


#### `updatePosition`

``` purescript
updatePosition :: forall eff t9332. Mario.Character -> Control.Monad.Eff.Eff (dom :: DOM.DOM | t9332) Prelude.Unit
```


#### `updatePositionP`

``` purescript
updatePositionP :: forall eff. Mario.Character -> Control.Monad.Eff.Eff (dom :: DOM.DOM | eff) Prelude.Unit
```


#### `jumpKeyCode`

``` purescript
jumpKeyCode :: Number
```


#### `rightKeyCode`

``` purescript
rightKeyCode :: Number
```


#### `leftKeyCode`

``` purescript
leftKeyCode :: Number
```


#### `groundHeight`

``` purescript
groundHeight :: Number
```




