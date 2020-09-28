# Pogger lang

In addition to traditional s-expression, I'm planning to add indentation based syntax wrapped with `{ }` or a new level of identation, and type annotation wrapped with `<>`. The indentation will be based on [this document](https://srfi.schemers.org/srfi-119/srfi-119.html)with some small modification.

#### Everything within a curly bracket will follow indentation based syntax.
```
-- use s expression
[:: fact (-> Int Int)]
define (fact n) {
  if : zero? n
    1
    * n : fact (- n 1)
}

-- use infix notation
-- :: has higher precedence than ->
[fact :: Int -> Int]
define : fact n
  if : zero? n
    1
    n * (fact (n - 1))

-- same as
(define (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))
```

Binary operartions like `+ :: a -> a -> a` in s-expression will be converted into `+f list = foldr (+) mempty list `. So when we're converting from s-expression back to infix notation, if there are more than two parameters for the biary operation it should be written in the foldr form.

```
[sum :: List Int -> Int]
define : sum xs
  foldr (+) 0 xs

-- same as
(define (sum xs) (apply + xs))

```

#### Curly bracket and normal bracket can be mixed and nested for readability, but it doesn't affect the semantics.
```
[:: fact : -> Int Int)]
define : (fact n) {
  if (zero? n) {
    1
    * n (fact : - n 1)
  }
}
```

```
)
[:: power-set {
  forall (a)
    -> Set a
       Set : Set a
}]
(define (power-set a) {
  if : = 0 (set-count xs)
    let
      : set1 (inst set a)
        set2 (inst set (Set a))
      set2 : set1
    let
      : xs- (power-set (set-rest xs))
      set-union
        for/set [e xs-]
                set-add e : set-first xs
                xs-
})
```

#### Support algebraic data type
```
-- GADT by default.

define-data : Maybe a {
  Just : a -> Maybe a
  Nothing : Maybe a
}

-- short hand
define-data : List a {
  Nil
  Cons a : List a
}

-- newtype wrapper
define-newtype : Chain a {
  Chain (List a)
}
-- derving is separated from type definition.
deriving Chain { Eq Show }

```

#### Support typeclass
```
define-class : Fuctor a
  map : (-> a b) -> f a -> f b

-- same as
(define-class (Functor a)
  ([map (-> (-> a b) (f a) (f b))]))

define-instance (FunctorMaybe : Functor Maybe)
  (define (map f ma) {
    match (ma)
      Nothing : Nothing
      Just a : Just (f a)
  })

-- same as
(define-instance (FunctorMaybe (Functor Maybe)
  (define (map f ma)
    (match (ma)
      ([Nothing Nothing]
       [Just a (Just (f a))])))))
```

#### do notation
```
[:: run (-> (Eff Unit))]
define : run {
  do
    a <- getArgs
    putStrLn a
}

[:: launch : Missile -> Eff Unit]
define : launch missile {
  do
    missileName <- missile.name   -- dot without space is member accessor
    putStrLn : intersperse " " . upper : missileName -- with space is comp
    launchMissile missile
}
```
