# Pogger lang

Pogger is a scheme like purely functinal language with haskell features. The language will be implemented in haskell and there will be a rust implemented runtime system resemble GHC's runtime system.

The surface syntax of pogger will first be compiled into system F ir. Then like ghc, it will be compiled to spinless tagless graph reduction machine, finaly to llvm ir.

#### Implementation road map
Follow the process of [implementation of functional language]()https://www.microsoft.com/en-us/research/wp-content/uploads/1992/01/student.pdf)
- Parser for pogger's surface syntax.
- A system F based core language.
- A compiler from pogger to core.
- A graph reduction machine.
- A compiler from core to graph reduce machine.
- LLVM JIT rts for now. plan to make a rust version.

#### S-expression without brackets
To make the syntax a bitter easier to write, I try to implement the [readable lisp s-expresion project](https://readable.sourceforge.io) and [this document](https://srfi.schemers.org/srfi-119/srfi-119.html). The goal is to support infix notation and indentation while preserve the s-expression semantics so we can use the same macro system as it is in traditional schemes. Some main idea from the readable s-expression project.

__Sweet expression__: Use indentation to replace unnecessary brackets.

__Bracket infix__: The readable sexp project uses [curly](curly) bracket to indicate the use of infix notation. No precedence support so the s expression structure is preserved. In progger both `{}` and `[]` will be used for infix bracket while `()` will be used for x expression brackets.

__Currying__:
I'll avoid the `neoteric expression ` because I want to support currying. so for function call like `{1 + }` will be a new function with type `Int -> Int` . Traditional lisp doesn't support currying by default. For scheme it's possible to achieve auto currying with macro like this
```scheme
(define-syntax define-curried
  (syntax-rules ()
   ((_ (f . a) body ...)
    (define f (curry (lambda a (begin body ...)))))))

(define-curried (add a b) (+ a b))
(define add5 (add 5))
```

#### Type system
##### Hindely milner type system with named typeclass.
- http://steshaw.org/hm/hindley-milner.pdf

Some ghc extensions plans to implement:

##### Multi parameter typeclass
-  https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf
##### RankNType
- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/putting.pdf
##### GADT
- https://www.seas.upenn.edu/~sweirich/papers/gadt.pdf

No plan for type families yet. Might try to do it when decide to do generic programming.

__basic__
```
[ fact : { Int -> Int } ]
define (fact n)
  if (zero? n)
    1
    { n * (fact { n - 1 })}
```

__inpace annotation__
```
-- inplace annotation
define (foo n)
  (+ [n : Int] 3)
-- the function will have type foo : Int -> Int
```

__ADT__
```
-- default ADT definition
define-data (Maybe a)
  Just a
  Nothing

(define-data (Maybe a) ((Just a) (Nothing)))

-- with GATD
define-data (Maybe a)
  [ Just a : { a -> Maybe a } ]
  [ Nothin : Maybe a ]

(define-data (Maybe a)
  (: (Just a) (-> a (Maybe a)))
  (: Nothing (Maybe a)))
```

__typeclass__
```
-- typeclass definition
define-class (Functor a)
  [ map : { { a -> b } -> (f a) -> (f b) } ]

-- equivalent s-expression form
(define-class (Functor a)
  (: map (-> (-> a b) (f a) (f b))))
```

__Named instance__
```
-- typeclass instance
define-instance FunctorMaybe (Functor Maybe)
  define (map f m)
    match m
      [Nothing => Nothing]
      [Just a => Just (f a)]

(define-instance FunctorMaybe (Functor Maybe)
  (define (map f m)
    (match (m) ((=> Nothing Nothing)
                (=> (Just a) (Just (f a)))))))
```

#### do notation as function
Pogger support do notation directly. A use of curly bracket is to make everything in it infix by default. Each indentation is a list, and the top level operator in that list will be infix. further nested operator still need bracket because there is no operator precedence.

```
[run : Eff Unit]
define run
  do {
    a <- getArgs
    readLine >>= |line| do {  -- lambda
      writeLine "line" line
      pure ()
    }
    putStrLn { a <> "some other string" } -- nested infix still need {}
  }
```

#### Everythings are functions
It looks like pogger needs to support special syntax like `->` and `do` , but the goal is everything should be a function, and if you like you can overload everything. `->` is just a function take a type and return another type, `:` is a function take a type annotation type and return a type annotation. `do` will take a list of operations as parameter; when execute it will insert `>>` between each lists to make it sequential. `<-` should be something reverse the order of parameters of `>>=`, but I'm not sure how to do that yet.

