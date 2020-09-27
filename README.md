# Ohjee lang

Lang playground.

```
-- Some random scratch. Nothing works here.

<> all prefix binary operation convert to infix ?
{} in addition of <>, use haskell style indentation,
   function application doesn't need parenthesis anymore ?
    what about if? works the same:
    <foo : Int -> Int>
    (define (foo x) {
      if (== 10) 20 { -- nest another {}
        20
      } else (+ 20 100)   -- just normal else
    })


All these syntax get convert to paranthesis, so macro sould work.

<foo : (a -> m a) -> a>
(define (foo a)
  (a b))

<bar : IO a>
(define bar
  (do (let ([x 10]
            [y 20])
      ((<- a getArgs)
       (<- b getArgs)
       (<- c getArgs)))
      (putStrLn (combine a b c))))

-- { }
<bar1 : IO a>
(define bar1
  (do {
        let x (+ 10 y)
            y 30
        a  <- getArgs
        b  <- getArg
        let y 30        -- how to achieve this ?
        c  <-  get
        putStrLn (combine a b c)
  }))

-- lambda with (\ ([x: Int] [y: Int]) { ... })
<goo : Show s => s -> Maybe s>
(define (good s)
  (\ () {
    match s {
      "string version 1"  xx
      "strig verions 2"
    }
    (show s == "good") ? Maybe --
  }))

<ff : forall a. (forall b -> b -> a) -> a> -- how to do this?
(define (ff f b)
  (undefined))
-- marco should work with {} and <>
-- everything in <> will become infix notation.
-- but essentially it's still list.

{
ff : forall a
  (forall b -> b -> a) -> a
}

(: ff (forall a . (forall b . (-> b b a)) a))

```
