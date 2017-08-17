---
title: "GHC Core by example, episode 1: Hello, Core!"
author: Alp Mestanogullari
date: 2013-06-27 03:00
comments: true
tags: haskell, ghc, core
description: This is the first post in a series where we take a closer look at GHC Core, the language to which Haskell gets desugared too in GHC. Very pragmatic approach.
---

GHC Core is well behind monads and burritos in the list of most documented topics. You can gather some knowledge from a few blog posts by Haskell gurus, some from StackOverflow or haskell-cafe and some from the GHC commentary or GHC's code itself. I will try from time to time to publish a simple case-study to see how your usual Haskell code gets translated to GHC Core, learning some bits of Core along the way. Sounds good? Then read on.

What's GHC Core
===============

As some of you may know, before generating native code for your platform (through the NCG or LLVM, with `-fllvm`), GHC compiles your Haskell code to several intermediate languages, as witnessed by the following diagram ([courtesy of the authors of the Haskell course at Stanford](http://www.scs.stanford.edu/11au-cs240h/notes/ghc-slides.html#(7)), most likely David Terei).

![GHC Pipeline](http://www.scs.stanford.edu/11au-cs240h/notes/pipeline.png)

In this series we will focus on Core, which is at a high enough level to see the correspondance with our Haskell code, but happens late enough in the compilation process to reflect what transformations took place, how lazy or strict some functions will be, etc. Hopefully this will help you (and myself, by trying to decently explain things I will surely learn a lot) gain a better understanding of how GHC understands your Haskell code.

By the way, if other people feel like writing such blog posts, please do so! Obviously there's a lot to say, a lot of situations to cover, etc. So feel free to write (and let me know about, if possible) your own _GHC Core by example_ posts.

Hello world in Core
===================

Let's consider this simple Haskell module, in a file named, say `Hello.hs`.

``` haskell
module Hello where

x :: Int
x = 4
```

To see the GHC Core code generated for the above Haskell code, simply run the following:

```
$ ghc -O2 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes Hello.hs
```

The `-ddump-simpl` flag asks GHC to print the generated Core after the transformation engine optimising Core programs has run on it. That engine is called the _simplifier_, thus the "-simpl". The other options I give `ghc` make the Core output more readable by stripping out a bunch of things while still retaining enough interesting information. You can try just `ghc -O2 -ddump-simpl hello-core.hs`. You're of course not forced to use `-O2` but that turns on quite a lot of transformations that we will be interested in, sooner or later.

You should get an output similar to the following.

```
[1 of 1] Compiling Hello           ( Hello.hs, Hello.o )

==================== Tidy Core ====================
Result size of Tidy Core = {terms: 3, types: 1, coercions: 0}
```

Followed by the actual Core code:

``` haskell
x :: Int
x = I# 4
```

The first part gives statistics about the generated core code, and the second part is the code itself. Core is quite similar to Haskell. We'll see quite a few differences, but it's nonetheless close to Haskell syntax. The `I#` there is just the name of the constructor for `Int`s, defined in GHC as (at least something equivalent to) `data Int = I# Int#`, where `Int#` is a machine integer, without any laziness on top.

How does a list get translated to Core? Let's find out by adding the following code to `Hello.hs`.

``` haskell
l :: [Int]
l = [1,2,3]
```

We get, stripping out the statistics and the Core for `x`:

``` haskell
l5 :: Int
l5 = I# 1

l4 :: Int
l4 = I# 2

l3 :: Int
l3 = I# 3

l2 :: [Int]
l2 = : l3 ([])

l1 :: [Int]
l1 = : l4 l2

l :: [Int]
l = : l5 l1
```

So it creates an `Int` for each number of that list, and one call to `(:)` for each tail in that list.  Let's see how a simple function gets translated now.

``` haskell
-- From now on, I'll write the haskell code in a first snippet, 
-- and the resulting Core in a second snippet right after
f :: Int -> Int
f x = x -- f = id for Ints
```

``` haskell
f :: Int -> Int
f = \ (x1 :: Int) -> x1
```

Ok, nothing scary so far. The only differences are that it puts the argument on the right, thus rewriting the function as equal to a lambda, explicitly, and it annotates the argument with a type in the code itself, not just using the type signature. Let's try two arguments now.

``` haskell
cst :: Int -> Int -> Int
cst x y = x -- f = const for Ints
```

``` haskell
cst :: Int -> Int -> Int
cst = \ (x1 :: Int) _ -> x1
```

Same as before, and it explicitly shows that the `y` argument isn't used by replacing it by `_` in the argument list of the lambda. Let's go polymorphic now, and actually give the definition for `id`, not an `Int` version.

``` haskell
id' :: a -> a -- also expressable as `forall a. a -> a`
id' x = x
```

``` haskell
id' :: forall a. a -> a
id' = \ (@ a) (x1 :: a) -> x1
```

Now it becomes interesting! We see that the type hasn't changed much, except that there's an explicit `forall` that 
introduces the type variable `a`. What may be more surprising though, is that **the type itself** explicitly becomes an **argument to the function**, with some special syntax though, getting a "@" prefix. The argument is then type-annotated using that "type argument". How about looking at the Core for a function equivalent to `flip (,)`?

``` haskell
reversePair :: a -> b -> (b, a)
reversePair a b = (b, a)
```

``` haskell
reversePair :: forall a b. a -> b -> (b, a)
reversePair = \ (@ a) (@ b) (a :: a) (b :: b) -> (b, a)
```

This time, we have two polymorphic arguments, not necessarily of the same type, and both of these type variables are (type-)arguments to that function and used when annotating the type of the actual arguments.

How about taking functions as arguments?

``` haskell
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x) -- compose = (.)
```

``` haskell
Hello.compose :: forall b c a. (b -> c) -> (a -> b) -> a -> c
Hello.compose =
  \ (@ b) (@ c) (@ a) (f1 :: b -> c) (g :: a -> b) (x1 :: a) ->
    f1 (g x1)
```

Nothing really fun here, just what you would expect considering everything we've seen so far. Let's move on to something more interesting.

# Typeclasses

You may have noticed that no typeclass has been involved so far, in any way. That's deliberate: the Core gets a bit hairy when typeclasses are involved. Let's see what I mean by first looking at what the Core is for a simple call to a typeclass method (`Num`'s `(*)` in this case) with a specific type, not with the general `Num a => a -> a` type.

``` haskell
square :: Int -> Int
square x = x*x
```

``` haskell
square :: Int -> Int
square = \ (x1 :: Int) -> $fNumInt_$c* x1 x1
```

Ew! `$fNumInt_$c*`, really?! Yes, but don't worry, you get used to this pretty quickly. So this is what the call to `(*)` gets translated to. If you remove `-dsuppress-module-prefixes` from the `ghc` command, you'll get `GHC.Num.$fNumInt_$c*`. That means the definition of `(*)` for `Int`s is
in the `GHC.Num` module, as a function that gets a weird name in Core. We see that it contains the name of the class (`Num`), the name of the type (that has an instance for that class - `Int` here), and the name of the typeclass method (here it's `*`, without the parens -- in Core, even operators appear in prefix position, just like any function).

What if we define our own typeclass and write an instance for some type?

``` haskell
class Foo a where
  foo :: a -> Char

instance Foo Bool where
  foo True = 't'
  foo False = 'f'
```

``` haskell
foo :: forall a. Foo a => a -> Char
foo = \ (@ a) (tpl :: Foo a) -> tpl `cast` ...

$fFooBool2 :: Char
$fFooBool2 = C# 'f'

$fFooBool1 :: Char
$fFooBool1 = C# 't'

$fFooBool_$cfoo :: Bool -> Char
$fFooBool_$cfoo =
  \ (ds :: Bool) ->
    case ds of _ {
      False -> $fFooBool2;
      True -> $fFooBool1
    }

$fFooBool :: Foo Bool
$fFooBool = $fFooBool_$cfoo `cast` ...
```

There are some weirds things going on now. First, we see two mysterious calls to some `cast` function here. We'll get back to it later. The two `Char`s just define `'f'` and `'t'` in terms of the internal representation of `Char`, just like we had with `I#` earlier. Even these auxiliary values are prefixed with where they come from (`Bool` instance of the `Foo` typeclass). `fFooBool_$cfoo` is the Core name of the implementation of `foo` from the `Foo` typeclass for the `Bool` instance, and the content of the function is pretty straightforward, except for the weird way the `case` line looks.

But hey, what's that: `$fFooBool :: Foo Bool`? `Foo` is a typeclass, so how can `Foo Bool` be a type? Well, typeclass instances happen to be implemented as records of values (may they be functions or just simple values), and a typeclass defines that data type (we call these *dictionaries*). Then each instance is just a value of that type. So you can picture our `Foo` class as generating a type `Foo a` equivalent to `data Foo a = Foo { foo :: a -> Char }`. And then the `Bool` instance is just a value of that type, with the `foo` field set to `$fFooBool`, in some way. Note that this generalizes to several methods/values in your typeclass, you just extend the data type with more fields.

But what are these `cast`s? Let's remove `-dsuppress-coercions` to figure out what's happening.

``` haskell
foo :: forall a. Foo a => a -> Char
foo =
  \ (@ a) (tpl :: Foo a) ->
    tpl `cast` (<NTCo:Foo <a>> :: Foo a ~# (a -> Char))

-- ...

$fFooBool :: Foo Bool
$fFooBool =
  $fFooBool_$cfoo
  `cast` (Sym <(NTCo:Foo <Bool>)> :: (Bool -> Char) ~# Foo Bool)
```

- The first one, without the `Sym`, takes a *dictionary* and kind of projects it to get the foo function, and it does so generically by taking as arguments both the `a` type and the `tpl` argument, which is the dictionary (or typeclass instance/implementation, if you prefer) for the given type `a`. That's the actual function you call when you write a call to `foo` in your Haskell code, initially. You can note here that `forall a. Foo a => ...` gets desugared to having as arguments the type and the dictionary.
- The second one kind of does the inverse of the first: it creates a dictionary `$fFooBool` from the definition of `foo` for our `Bool` instance, `$fFooBool_$cfoo`.

So typeclass constraints on values/functions are just translated to passing some dictionary arguments in Core! Let's see how that's processed when we actually write a typeclass constraint in our code, stripping away coercions again this time (since we now have seen how the casts look like).

``` haskell
f :: Foo a => a -> [Char]
f x = [foo x, foo x]

g :: Bool -> [Char]
g b = f b
```

``` haskell
f_$sf :: Bool -> [Char]
f_$sf =
  \ (x :: Bool) ->
    : (case x of _ {
         False -> $fFooBool2;
         True -> $fFooBool1
       })
      (: (case x of _ {
            False -> $fFooBool2;
            True -> $fFooBool1
          })
         ([]))

f :: forall a. Foo a => a -> [Char]
f =
  \ (@ a) ($dFoo :: Foo a) (x :: a) ->
    : (($dFoo `cast` ...) x) (: (($dFoo `cast` ...) x) ([]))

g :: Bool -> [Char]
g = f_$sf
```

We can witness that this _type class constraint -> dictionary argument_ translation has been done again, for `f`. Note that these calls to `cast` on `$dFoo` are just this GHC Core's `foo` (we mentionned it above, it has a `cast` call) calls in `f` being inlined. So `$dFoo ...` here really just means the `foo` implementation for the given `a` type. You can also see that GHC created a rule that specifies that any call to `f` on a `Foo Bool` dictionary (meaning: whenever we use the `Foo` instance for `Bool`), should be rewritten as a call to (the GHC-generated) `f_$sf` function, which, as you can see, has accomplished quite some inlining without us asking.

``` haskell
------ Local rules for imported ids --------
"SPEC f [Bool]" [ALWAYS]
    forall ($dFoo :: Foo Bool). f $dFoo = f_$sf
``` 

That's going to be all for now. Next time we'll see how some more useful code gets translated to Core.
