---
title: Coyoneda and fmap fusion
author: Alp Mestanogullari
date: 2017-08-17 17:00
description: The (Co)Yoneda lemma from category theory can speed up Haskell programs.
comments: true
tags: haskell, yoneda
---

Let's quickly see how the (dual variant of the) Yoneda lemma can speed up
some Haskell programs -- more specifically ones that are repeatedly calling
`fmap` to transform some data within a `Functor`.

We will be focusing on the following `Functor`:

``` haskell
data Tree a
  = Bin a (Tree a) (Tree a)
  | Nil
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Nil         = Nil
  fmap f (Bin a l r) = Bin (f a) (fmap f l) (fmap f r)

-- we'll also use this instance to perform a silly computation
-- on our tree after some transformations have occured
instance Foldable Tree where
  foldMap _ Nil         = mempty
  foldMap f (Bin a l r) = f a <> foldMap f l <> foldMap f r
```

A simple binary tree with data stored in the nodes, whose
`Functor` instance lets us map a function over each `a`
stored in our tree and whose `Foldable` instance lets us
combine computations performed over our `a`s.

# Coyoneda

The Coyoneda lemma, when interpreted on haskell `Functor`s, tells us that
`Coyoneda f a` is equivalent (isomorphic) to `f a`, where `Coyoneda` is:

``` haskell
data Coyoneda f a where
  Coyoneda :: (b -> a) -> f b -> Coyoneda f a
```

We see that it holds an `f b` and a way to go from `b`s to `a`s,
effectively making it equivalent to `f a` if you `fmap` the first
field on the second one. That's also the only sensible thing we can
do with such a value, as the `b` is hidden.

If it's equivalent to `f a`, it must be a `Functor` too? Sure enough it is.

``` haskell
instance Functor (Coyoneda f) where
  fmap f (Coyoneda b2a fb) = Coyoneda (f . b2a) fb
```

We see that calling `fmap f` amounts to "accumulating" more work in the
`b -> a` field, possibly even changing from a given `a` to some other type,
as allowed by `fmap`. This is exactly the piece of code that powers "fmap fusion".
Instead of going from `f a` to `f b` with `fmap f` and then to `f c` with `fmap g`,
the `Coyoneda` representation keeps hold of the original `f a`, which is left untouched
by the `Functor` instance from above, and instead simply composes `f` and `g` in that
first field.

Now, we said that `f a` and `Coyoneda f a` are isomorphic but did not provide
functions to prove our claim, let's fix that right away.

``` haskell
coyo :: f a -> Coyoneda f a
coyo = Coyoneda id

uncoyo :: Functor f => Coyoneda f a -> f a
uncoyo (Coyoneda f fa) = fmap f fa
```

Note that we do not need `f` to be a `Functor` to build a
`Coyoneda f a`, as there's no need to call `fmap` until the very
end, when we have composed all our transformations and finally
want to get the final result as some `f a`, not `Coyoneda f a`.

Maybe it's still not clear to you that successive fmap calls are fused, so
let's _prove_ it. We want to show that for two functions `f :: b -> c` and
`g :: a -> b`, `uncoyo . fmap f . fmap g . Coyoneda id = fmap (f . g)`.

``` haskell
uncoyo . fmap f . fmap g . coyo
  = uncoyo . fmap f . fmap g . Coyoneda id -- definition of coyo
  = uncoyo . fmap f . Coyoneda (g . id)    -- Functor instance for Coyoneda
  = uncoyo . fmap f . Coyoneda g           -- g . id = g
  = uncoyo . Coyoneda (f . g)              -- Functor instance for Coyoneda
  = fmap (f . g)                           -- definition of uncoyo
```

Nice! And you could of course chain any number of `fmap` calls and they would
all get fused into a single `fmap` call that applies the composition of all the
functions you wanted to `fmap`.

For instance, back to our tree, let's define some silly computations:

``` haskell
-- sum all the values in a tree
sumTree :: Num a => Tree a -> a
sumTree = getSum . foldMap Sum

-- an infinite tree with integer values
t :: Tree Integer
t = go 1

  where go r = Bin r (go (2*r)) (go (2*r + 1))

-- only keep the given number of depth levels of
-- the given tree
takeDepth :: Int -> Tree a -> Tree a
takeDepth _ Nil = Nil
takeDepth 0 _   = Nil
takeDepth d (Bin r t1 t2) = Bin r (takeDepth (d-1) t1) (takeDepth (d-1) t2)

-- a chain of transformations to apply to our tree
transform :: (Functor f, Num a) => f a -> f a
transform = fmap (^2) . fmap (+1) . fmap (*2)
```

Now with a simple `main` we can compare how efficient
it is to compute `sumTree $ takeDepth n (transform t)`
by using `Tree` vs `Coyoneda Tree` as the functor on which
the transformations are applied. You can find an executable
module in [this gist](https://gist.github.com/alpmestan/62cfef6076800a27042fe59f6b1fb8b0).

If we compare with and without Coyoneda for `n = 23`, there's already
a noticeable (and reproducible) difference:

```
$ time ./yo 23
787061080478271406079

real0m3.968s
user0m3.967s
sys0m0.000s

$ time ./yo 23 --coyo
787061080478271406079

real0m2.384s
user0m2.380s
sys0m0.003s
```
