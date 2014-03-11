---
title: A simple GLFW-b example
author: Alp Mestanogullari
date: 2013-07-17 03:00
comments: true
description: I just happened to have some free time and heard about GLFW-b and ported the official C example to Haskell.
tags: haskell, opengl, glfw
---

There are some really clever and productive people on the [#haskell-game](irc://irc.freenode.net/haskell-game) IRC channel. Among them is [Brian Lewis](https://github.com/bsl), who maintains a binding to the [GLFW](http://www.glfw.org) library, which lets you create windows with OpenGL contexts in them and manage inputs and events. His binding is called [GLFW-b](http://hackage.haskell.org/package/GLFW-b) and is just a `cabal install` away from you.

![Screenshot](http://alpmestan.com/glfw.png)

Recently, Brian updated his package to match GLFW 3.0, so I thought it could be a good opportunity to see how it felt to use his package. I decided to port [the official "quick example"](http://www.glfw.org/docs/3.0/quick.html#quick_example) in Haskell using GLFW-b, and here's the reulting code. I tried to keep the same spirit as in the original code, just introducing some utility functions to make that code smoother to read.

``` haskell
module Main where

import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL
import qualified Graphics.UI.GLFW as G
import System.Exit
import System.IO

-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit easier to read

bool :: Bool -> a -> a -> a
bool b falseRes trueRes = if b then trueRes else falseRes

unless' :: Monad m => m Bool -> m () -> m ()
unless' action falseAction = do
    b <- action
    unless b falseAction

maybe' :: Maybe a -> b -> (a -> b) -> b
maybe' m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: G.ErrorCallback
errorCallback err description = hPutStrLn stderr description

keyCallback :: G.KeyCallback
keyCallback window key scancode action mods = when (key == G.Key'Escape && action == G.KeyState'Pressed) $
  G.setWindowShouldClose window True

main :: IO ()
main = do
  G.setErrorCallback (Just errorCallback)
  successfulInit <- G.init
  -- if init failed, we exit the program
  bool successfulInit exitFailure $ do
      mw <- G.createWindow 640 480 "Simple example, haskell style" Nothing Nothing
      maybe' mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          G.setKeyCallback window (Just keyCallback)
          mainLoop window
          G.destroyWindow window
          G.terminate
          exitSuccess
          
mainLoop :: G.Window -> IO ()
mainLoop w = unless' (G.windowShouldClose w) $ do
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    clear [ColorBuffer]
    
    matrixMode $= Projection
    loadIdentity
    ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    matrixMode $= Modelview 0
    
    loadIdentity
    -- this is bad, but keeps the logic of the original example I guess
    Just t <- G.getTime
    rotate ((realToFrac t) * 50) $ (Vector3 0 0 1 :: Vector3 GLdouble)
    
    renderPrimitive Triangles $ do
        color  (Color3 1 0 0 :: Color3 GLdouble)
        vertex (Vertex3 (negate 0.6) (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 1 0 :: Color3 GLdouble)
        vertex (Vertex3 0.6 (negate 0.4) 0 :: Vertex3 GLdouble)
        color  (Color3 0 0 1 :: Color3 GLdouble)
        vertex (Vertex3 0 0.6 0 :: Vertex3 GLdouble)
        
    G.swapBuffers w
    G.pollEvents
    mainLoop w
```

The code [sits in a github repo](https://github.com/alpmestan/glfw-b-quick-example) for your viewing/forking pleasure.