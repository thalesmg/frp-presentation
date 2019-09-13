{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Main where

import Lib

import Graphics.Gloss.Rendering
import Control.Monad.Fix (MonadFix)
import FRP.Netwire
import Control.Wire

main :: IO ()
main = do
  glossState <- initState
  withWindow width height "Kabum" $ \win ->
    Lib.loop win (update2, clockSession_, (5, 5)) glossState

update1 :: (HasTime t s, MonadFix m) => Wire s () m Vec Vec
update1 = proc _ -> do
  a <- acceleration1 -< ()
  v <- velocity1 -< a
  p <- position  -< v
  returnA -< p

update2 :: (HasTime t s, MonadFix m) => Wire s () m Vec Vec
update2 = proc _ -> do
  a <- acceleration1 -< ()
  v <- velocity2 -< a
  p <- position  -< v
  returnA -< p

update3 :: (HasTime t s, MonadFix m) => Wire s () m Vec Vec
update3 = proc _ -> do
  a <- acceleration2 -< ()
  v <- velocity1 -< a
  p <- position  -< v
  returnA -< p

vx0 = 70
vy0 = 0

vx1 = 70
vy1 = 50

acceleration1 :: (Monad m) => Wire s () m () Vec
acceleration1 = pure (0, 0)

acceleration2 :: (Monad m) => Wire s () m () Vec
acceleration2 = pure (100, 200)

velocity1 :: (HasTime t s, Monad m) => Wire s () m Vec Vec
velocity1 = proc (ax, ay) -> do
  vx <- integral vx0 -< ax
  vy <- integral vy0 -< ay
  returnA -< (vx, vy)

velocity2 :: (HasTime t s, Monad m) => Wire s () m Vec Vec
velocity2 = proc (ax, ay) -> do
  vx <- integral vx1 -< ax
  vy <- integral vy1 -< ay
  returnA -< (vx, vy)

position :: (HasTime t s, Monad m) => Wire s () m Vec Vec
position = proc (vx, vy) -> do
  px <- integral 0 -< vx
  py <- integral 0 -< vy
  returnA -< (px, py)
