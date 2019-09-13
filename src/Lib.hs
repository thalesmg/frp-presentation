{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Lib where

import Control.Monad.Fix (MonadFix)
import qualified Graphics.Gloss.Rendering as GR
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import FRP.Netwire (Wire, HasTime, Session, Timed, returnA, integral, clockSession_, stepSession)
import Control.Wire (NominalDiffTime, stepWire, time, (***), (>>>))

type MyState = (Wire (Timed NominalDiffTime ()) () IO Vec Vec, Session IO (Timed NominalDiffTime ()), (Float, Float))
type Vec = (Float, Float)

width = 640
height = 480
pHeight = 20
pWidth = 20

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      Nothing -> pure ()
      Just win -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
    GLFW.terminate
  where
    simpleErrorCallback :: GLFW.Error -> String -> IO ()
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

keyIsPressed :: GLFW.Window -> GLFW.Key -> IO Bool
keyIsPressed win key = isPress <$> GLFW.getKey win key

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed = True
isPress GLFW.KeyState'Repeating = True
isPress _ = False

loop :: GLFW.Window -> MyState -> GR.State -> IO ()
loop win state glossState = do
  let (wire, session, pos) = state
  (ds, session') <- stepSession session
  (pos', wire') <- stepWire wire ds (Right pos)
  let pos'' = either (const (0, 0)) id pos'
  threadDelay (2 * 10^4)
  GLFW.pollEvents
  esc <- keyIsPressed win GLFW.Key'Escape
  let newState = (wire', session', pos'')
  renderFrame pos'' win glossState
  unless (esc || fst pos'' > fromIntegral width / 2) $ loop win newState glossState

renderFrame :: Vec -> GLFW.Window -> GR.State -> IO ()
renderFrame st win glossState = do
  let (x, y) = (realToFrac *** realToFrac) st
  GR.displayPicture (width, height) black glossState 1.0 $
    translate x y $ color orange $ rectangleSolid pHeight pWidth
  GLFW.swapBuffers win
