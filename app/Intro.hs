{-# LANGUAGE OverloadedStrings #-}

module Intro
  ( inputIntro,
    tickIntro,
    renderIntro,
  )
where

import Constants (introScreenTime)
import Control.Lens
import InitialStates (initialMainMenuState)
import qualified SDL
import System.Random (getStdGen)
import Types
import UtilsSDL

inputIntro :: SDL.EventPayload -> Assets -> IntroState -> IO MainStatePhase
inputIntro ev assets is =
  case ev of
    SDL.KeyboardEvent evData ->
      -- Pressing any key skips the intro.
      if SDL.keyboardEventKeyMotion evData == SDL.Pressed
        then endIntro assets
        else return (Intro is)
    _ -> return (Intro is)

endIntro :: Assets -> IO MainStatePhase
endIntro assets =
  return $ MainMenu initialMainMenuState

tickIntro :: Double -> Assets -> IntroState -> IO MainStatePhase
tickIntro dt assets is =
  if is ^. introTime >= introScreenTime
    then endIntro assets
    else return $ Intro (is & introTime +~ dt)

renderIntro :: SDL.Renderer -> Assets -> IntroState -> IO ()
renderIntro renderer assets _ =
  do
    renderText (assets ^. textAssets . font) renderer "Tetris 1985-2020 Tetris Holding." (10, baseY)
    renderText (assets ^. textAssets . font) renderer "Tetris logos, Tetris theme song and Tetriminos" (10, baseY + 24)
    renderText (assets ^. textAssets . font) renderer "are trademarks of Tetris Holding." (10, baseY + 48)
    renderText (assets ^. textAssets . font) renderer "The Tetris trade dress is owned by Tetris Holding." (10, baseY + 72)
    renderText (assets ^. textAssets . font) renderer "Licensed to The Tetris Company." (10, baseY + 96)
    renderText (assets ^. textAssets . font) renderer "Tetris Game Design by Alexey Pajitnov." (10, baseY + 120)
    renderText (assets ^. textAssets . font) renderer "Tetris Logo Design by Roger Dean." (10, baseY + 144)
    renderText (assets ^. textAssets . font) renderer "All Rights Reserved." (10, baseY + 168)
  where
    baseY = 250
