{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Main where

import Lib

import Control.Monad.Fix (MonadFix)
import qualified Graphics.Gloss.Rendering as GR
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import qualified Graphics.UI.GLFW as GLFW
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import FRP.Netwire (Wire, HasTime, Session, Timed, returnA, integral, clockSession_, stepSession)
import Control.Wire (NominalDiffTime, stepWire, time, (***), (>>>))

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

width = 640
height = 480
pHeight = 20
pWidth = 20
vx0 = 50
vy0 = 0

type MyState = (Wire (Timed NominalDiffTime ()) () IO Vec Vec, Session IO (Timed NominalDiffTime ()), (Float, Float))
type Vec = (Float, Float)

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

update :: (HasTime t s, MonadFix m) => Wire s () m Vec Vec
update = proc _ -> do
  a <- acceleration -< ()
  rec p <- position -< v
      v <- velocity -< a
  returnA -< p

acceleration :: (Monad m) => Wire s () m () Vec
acceleration = pure (0, 0)

velocity :: (HasTime t s, Monad m) => Wire s () m Vec Vec
velocity = proc (ax, ay) -> do
  vx <- integral vx0 -< 0
  vy <- integral vy0 -< 0
  returnA -< (vx, vy)

position :: (HasTime t s, Monad m) => Wire s () m Vec Vec
position = proc (vx, vy) -> do
  px <- integral 0 -< vx
  py <- integral 0 -< vy
  returnA -< (px, py)

main :: IO ()
main = do
  glossState <- GR.initState
  withWindow width height "Kabum" $ \win ->
    loop win (update, clockSession_, (5, 5)) glossState
