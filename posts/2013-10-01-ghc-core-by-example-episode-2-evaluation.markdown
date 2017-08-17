---
title: "GHC Core by example, episode 2: Evaluation"
author: Alp Mestanogullari
date: 2013-10-01 23:00
comments: true
description: Second post in my introduction to GHC Core, presented from a concrete point of view.
tags: haskell, ghc, core, evaluation
---

This post is the second episode of my *GHC Core by example* series. See the first one [here](http://alpmestan.com/posts/2013-06-27-ghc-core-by-example-episode-1.html).

## What do you mean, "evaluation" ?

You are probably aware, whatever your level is, that Haskell is really different from the usual mainstream languages. One of the key differences is that evaluation happens "as needed". To know more about this, read about *lazy evaluation* (I have a work-in-progress article [here](http://github.com/alpmestan/oh-my-laziness)).

It means that most values in your program will first be a thunk until they are actually required, for the evaluation of another expression in which the value appears generally. Every Haskell code compiled with GHC gets translated to Core, so can we see evaluation appear more explicitly in Core? Well, yes. Let's see how that goes.

## A first example

Let's just look at a simple function, in a `Eval.hs` module.

``` haskell
f :: Int -> Int
f x = x + 1
```

Let's inspect the generated core with the following options.

``` 
$ ghc -O2 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes Eval.hs
``` 

We get:

``` haskell
f :: Int -> Int
f = \ (x :: Int) -> case x of _ { I# x1 -> I# (+# x1 1) }
```

So the argument being an `Int`, it's a *boxed* integer, thus a normal Haskell value. That means that if `x` hasn't been evaluated yet, the evaluation of the expression yielding `x` will happen through that `case x of ...` expression. 

Wait, we are talking about that `case` expression, but it looks quite different than what we are used to. Generally, there is no `_` after the `of` keyword. Why is there one there? That's just a small detail, but let's get this out of the way so we can get used to it easily. The general pattern is:

``` haskell
case expr of ident { ... }
```

where `ident` will just bind to the evaluated `expr` so that you can refer to it on the right hand side of the different clauses of your `case ... of` expressions. For example:

``` haskell
-- for some list 'xs'
case null xs of b {
    True -> error "boo empty list"
    _    -> b
    }
-- yes, this is a bad function
```

`Int` is defined by `data Int = I# Int#` where `Int#` is the actual machine integer type, unboxed and all. So we really see here that the `case ... of` "pattern matches" on the `Int`, which has only one constructor. However, the value stored inside being an *unboxed* integer means that it's really sitting there, there's no laziness going on there. So we actually have fully evaluated that Int. After that, we perform the native `Int#` addition with `(+#)` and repack that into a `Int` with the `I#` constructor.

## Maybe a second?

Let's introduce another level of laziness.

``` haskell 
g :: Maybe Int -> Maybe Int
g Nothing  = Nothing
g (Just x) = Just (x+1)
```

We're basically doing the same, except that, this time, we're adding 1 to an `Int` wrapped in `Maybe`. What does the Core for that look like?

``` haskell
g :: Maybe Int -> Maybe Int
g =
  \ (ds :: Maybe Int) ->
    case ds of _ {
      Nothing -> Nothing;
      Just x -> Just (case x of _ { I# x1 -> I# (+# x1 1) })
    }
```

So, when passed a `Maybe Int` value, `g` will evaluate whether that value is `Nothing` or `Just <some int>`. That's achieved by the first (outermost) `case ... of`. The inner `case ... of` is the place where we actually evaluate what the value of that `Int` is (it could be a non-terminating computation like `product [1..]`, we don't know yet and didn't need to). You even may have noticed that the inner `case` is identical to the Core we saw for `f` earlier. 

I haven't mentionned this yet, but you may have noticed that we initially were doing the pattern matching by writing two 
equations, one for each constructor of `Maybe`, whereas it's a `case ... of` in the Core code: that's how it always ends up being rewritten in Core, meaning our Haskell version is just some syntactic sugar around a `case ... of`.

And your everyday haskell code gets evaluated the very same way, evaluating outermost constructors, and then going deep inside algebraic data types *as much as necessary for the calling code*, not more. Everything you compute in your Haskell code boils down to `case ... of` expressions in Core, **that's how values are evaluated**.

Consider this code:

``` haskell
h :: StateT [Int] [] String
h = do
  s <- get
  x <- lift s
  put $ map (+5) s
  return $ show x
```

Quite short right? Here's the Core.

``` haskell
lvl :: Int -> Int
lvl = \ (ds :: Int) -> case ds of _ { I# x -> I# (+# x 5) }

Rec {
h_go :: [([Int], [Int])] -> [(String, [Int])]
h_go =
  \ (ds :: [([Int], [Int])]) ->
    case ds of _ {
      [] -> [];
      : y ys ->
        case y of _ { (a5, s') ->
        let {
          eta :: [Int]
          eta = map lvl a5 } in
        letrec {
          go :: [(Int, [Int])] -> [(String, [Int])]
          go =
            \ (ds1 :: [(Int, [Int])]) ->
              case ds1 of _ {
                [] -> [];
                : y1 ys1 ->
                  let {
                    a1 :: String
                    a1 =
                      case y1 of _ { (a2, s'1) ->
                      case a2 of _ { I# ww -> $wshowSignedInt 0 ww ([]) }
                      } } in
                  letrec {
                    $sgo :: () -> [Int] -> [((), [Int])] -> [(String, [Int])]
                    $sgo =
                      \ _ (sc1 :: [Int]) (sc2 :: [((), [Int])]) -> : (a1, sc1) (go1 sc2);
                    go1 :: [((), [Int])] -> [(String, [Int])]
                    go1 =
                      \ (ds2 :: [((), [Int])]) ->
                        case ds2 of _ {
                          [] -> [];
                          : y2 ys2 -> : (a1, case y2 of _ { (a2, s'1) -> s'1 }) (go1 ys2)
                        }; } in
                  case $sgo () eta ([]) of _ {
                    [] -> go ys1;
                    : x xs -> : x (++ xs (go ys1))
                  }
              }; } in
        letrec {
          go1 :: [Int] -> [(Int, [Int])]
          go1 =
            \ (ds1 :: [Int]) ->
              case ds1 of _ {
                [] -> [];
                : y1 ys1 -> : (y1, s') (go1 ys1)
              }; } in
        case go (go1 a5) of _ {
          [] -> h_go ys;
          : x xs -> : x (++ xs (h_go ys))
        }
        }
    }
end Rec }

h1 :: [Int] -> [(String, [Int])]
h1 = \ (eta :: [Int]) -> h_go (: (eta, eta) ([]))

h :: StateT [Int] [] String
h = h1 `cast` ...
```

This is all complicated, because GHC inlines functions quite agressively (in particular with `-O2` -- there are really noticeable differences in the Core between `-O0` and `-O2`, or even `-O1`). But that was just to show you that everything in Core is really about function calls (including *constructors*) and case expressions. Among other things, we can see for example that GHC named our `(+5)` function (it's `lvl` in the Core). The structure of this Core code is the result of mixing the original definitions of the functions and the way GHC's inliner works.

That is all for this second episode. It's quite short but I wanted to get evaluation out of the way in order to emphasize on important transformations GHC does at the Core level in later posts. I apologize for the delay, I will most likely push new posts in this series more regularly.
