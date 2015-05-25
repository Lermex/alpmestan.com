---
title: Haskell
author: Alp Mestanogullari
comments: true
toc: true
---

**Haskell** has now been my programming language of choice for 7 years. For many things, I had to struggle to understand
the meat of them and went through the usual troubles (lost in monad tutorials and friends, seeing super-abstract 
code written mentionning all kinds of abstractions, etc). This page is just supposed to gather some links and explanations that I found to be helpful and/or interesting.

# Basics

## Books

Two good books that are recommended to any Haskell beginner:

- [Learn You A Haskell!](http://www.learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.com/)

In addition, I also suggest taking an occasional look at the [Haskell wikibook](http://en.wikibooks.org/wiki/Haskell) when the other two leave you clueless about something.

## Key topics not covered enough in the books

- [Laziness and strictness](http://alpmestan.com/posts/2013-10-02-oh-my-laziness.html) are covered in decent depth in this post I wrote on this blog ; you can find some great explanations in Simon Marlow's [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html).

- There are many monad tutorials out there, yes. There's one that I really recommend to *any* Haskeller who is still struggling to understand what they are, written by the great Dan Piponi: [You Could Have Invented Monads! (And Maybe You Already Have.)](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)

# GHC Core

Although I have written a few things about it (see [here](http://alpmestan.com/tags/core.html)), I strongly recommend checking out the links given on StackOverflow, mostly by Don Stewart: [Reading GHC Core](http://stackoverflow.com/questions/6121146/reading-ghc-core). 

A small tip for getting a GHC Core output that's actually readable is to build your file/project with:

``` shell
$ ghc -O2 -ddump-simpl -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-uniques -dsuppress-module-prefixes Hello.hs
```

# Libraries

- [Great tutorial about the 'reflection' library](https://www.fpcomplete.com/user/thoughtpolice/using-reflection), written by Austin Seipp.
- [An introductory talk about 'lens'](https://skillsmatter.com/skillscasts/4251-lenses-compositional-data-access-and-manipulation), by Simon Peyton Jones

# History

Like anything but photons in this world, *Haskell* does have an history. A very interesting one -- you can see many of the ideas and features we know, love and use these days come into play (but not all of them). And guess what? Most of that story is available to anyone interested in a very-well written paper.

- [A History of Haskell: Being Lazy With Class](http://research.microsoft.com/en-us/um/people/simonpj/papers/history-of-haskell/history.pdf), Paul Hudak (Yale University), John Hughes (Chalmers University), Simon Peyton Jones (Microsoft Research), Philip Wadler (Edinburgh University), The Third ACM SIGPLAN History of Programming Languages Conference (HOPL-III) San Diego, California, June 9-10, 2007.