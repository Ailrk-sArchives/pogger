# Pogger Scheme

In addition to traditional s-expression, I'm planning to add indentation based syntax wrapped with `{ }`, and type annotation wrapped with `<>`. The indentation will be based on [this document](https://srfi.schemers.org/srfi-119/srfi-119.html)with some small modification.

#### Everything within a curly bracket will follow indentation based syntax.
```
[:: fact (-> Int Int)]
(define (fact n) {
  if : zero? n
    1
    * n : fact (- n 1)
})
```

#### Curly bracket and normal bracket can be mixed and nested for readability, but it doesn't affect the semantics.
```
[:: fact (-> Int Int)]
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

(define )
```

#### Support typeclass
```
(define-class (Fuctor a) {
  [:: map . -> (-> a b) : f a : f b]
})

(define-instance (FunctorMaybe : Functor Maybe) {
  (define (map f ma) {
    match (ma)
      Nothing
        : Nothing
      Just a
        : Just : f a
  })
})
```
