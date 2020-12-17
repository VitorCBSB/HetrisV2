{-# LANGUAGE OverloadedStrings #-}

module Paused
  ( inputPaused,
    tickPaused,
    renderPaused,
  )
where

import Constants (windowSize)
import Control.Lens
import Game (renderGame)
import InitialStates (initialCountingDownState)
import qualified SDL
import Types
import UtilsSDL (renderTextCentered)

inputPaused :: SDL.EventPayload -> Assets -> PauseState -> IO MainStatePhase
inputPaused ev assets ps =
  case ev of
    SDL.KeyboardEvent evData ->
      if SDL.keyboardEventKeyMotion evData == SDL.Pressed && (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeEscape
        then do
          let (initCountdown, countSfx) = initialCountingDownState (assets ^. soundAssets) (ps ^. backgroundGS)
          applySideEffect countSfx
          return $ CountingDown initCountdown
        else return (Paused ps)
    _ -> return (Paused ps)

tickPaused :: Double -> PauseState -> IO MainStatePhase
tickPaused _ ps = return $ Paused ps

renderPaused :: SDL.Renderer -> Assets -> PauseState -> IO ()
renderPaused renderer assets ps =
  do
    renderGame renderer assets (ps ^. backgroundGS)
    renderTextCentered (assets ^. textAssets . font) renderer "PAUSED" (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2)