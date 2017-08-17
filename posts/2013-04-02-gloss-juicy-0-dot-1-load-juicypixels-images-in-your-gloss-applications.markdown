---
title: "gloss-juicy-0.1: Load JuicyPixels images in your gloss applications"
author: Alp Mestanogullari
date: 2013-04-02 15:28
comments: true
description: Having been a big fan of JuicyPixels since the beginning, and after having contributed a patch, I wrote a library that lets us load many formats of files inside gloss.
tags: haskell, gloss, JuicyPixels
---

Most of you probably know about [gloss](http://hackage.haskell.org/package/gloss) - that simple graphics library written by Ben Lippmeier. If you don't, and are interested in writing more or less simple 2D graphics applications, you really should check it out and kiss SDL goodbye.

On the other hand, there's [JuicyPixels](http://hackage.haskell.org/package/JuicyPixels), a simple but great library for encoding/decoding images, that as a side effect lets you convert between image formats, access the raw pixels from your images, and could possibly serve as a good basis for an image processing library entirely written in haskell.

One thing I wasn't really glad about is that despite being amazingly simple and well written, gloss only supports loading BMP images (it relies on Ben's [bmp](http://hackage.haskell.org/package/bmp) library) so I thought JuicyPixels could remedy that with some helper code. That's how [gloss-juicy was born](http://github.com/alpmestan/gloss-juicy), and I [released it to hackage](http://hackage.haskell.org/package/gloss-juicy) a few days ago.

![gloss-juicy in action](/images/io-monad.png)

Example
=======

I have written a simple image viewer that uses gloss-juicy, and decided it could serve as the perfect example to illustrate how to use it. If you [open the documentation](http://hackage.haskell.org/packages/archive/gloss-juicy/0.1/doc/html/Graphics-Gloss-Juicy.html), you will quickly see the API is pretty simple. Basically. if you already have a JuicyPixels image at hand, you can use [fromDynamicImage](http://alpmestan.com/gloss-juicy/doc/Graphics-Gloss-Juicy.html#g:1) if you don't know what types of pixels it has, or one of the _fromImage*_ functions. If you rather want to load the image from a file but don't know the format in advance, use [loadJuicy](http://alpmestan.com/gloss-juicy/doc/Graphics-Gloss-Juicy.html#g:2). If you _do know_ the format, use one of the _loadJuicy*_ functions. That's about it! 

So writing an image viewer should be pretty simple. First, a few imports:

``` haskell
module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Environment
```

First, let's get out of our way the function to display a gloss Bitmap.

``` haskell
displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = 
    display (InWindow "Image Viewer" (width, height) (10, 10))
            white
            p
displayPic _ = error "only the Bitmap constructor should be used here"
```

Our program will receive the filename to open through a command line argument. Let's start writing main, taking care of that command-line argument.

``` haskell
main :: IO ()
main = do
    args <- getArgs
    case args of 
        [filename] -> ???
        _          -> putStrLn "usage: gloss-juicy <file> -- displays the image in a gloss window"
```

What we want to do now is to use `loadJuicy` on the _filename_ and if it succeeds, call `displayPic` on it. That is:

``` haskell
loadJuicy filename >>= maybe (putStrLn $ "Couldn't load or decode " ++ filename) displayPic
```

Here is the full code:

``` haskell
module Main where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of 
        [filename] -> loadJuicy filename >>= maybe (putStrLn $ "Couldn't load or decode " ++ filename) displayPic
        _          -> putStrLn "usage: gloss-juicy <file> -- displays the image in a gloss window"


displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = 
    display (InWindow "Image Viewer" (width, height) (10, 10))
            white
            p
displayPic _ = error "only the Bitmap constructor should be used here"
```

To try it: `cabal update && cabal install gloss-juicy`. This will install _gloss-juicy_ in your ~/.cabal/bin. 

Implementation
==============

Gloss has a Picture data type, that basically represents anything that gloss can draw on your screen, and transformations on these things (e.g translations and rotations). My idea was to avoid an intrusive solution that would have forced me to modify gloss itself - maybe Ben wouldn't appreciate having to depend on Juicy.Pixels and all its dependencies, it would also be a quite major patch. Maybe we will discuss this in the future, but that is not in the works.

What I decided to do was rather to write conversion routines for almost all of Juicy.Pixels' image types to gloss' bmp image encoding. That means, of course, that the whole image has to be processed. Typically you would run that part while a shiny "Loading..." message would illuminate the eyes of your users.

Many thanks to Vincent Berthoux for his help on using Juicy.Pixels and on improving gloss-juicy (we also discussed a lot about the various performance improvements we could make), Ben and Lennart Kolmodin for being very reactive and helpful with my questions/requests.
