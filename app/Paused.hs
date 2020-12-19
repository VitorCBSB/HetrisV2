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
import InitialStates (initialCountingDownState, initialGameState)
import qualified SDL
import Types
import UtilsSDL (renderTextCentered)

inputPaused :: SDL.EventPayload -> Assets -> PauseState -> IO (MainStatePhase, Bool)
inputPaused ev assets ps =
  case ev of
    SDL.KeyboardEvent evData
      -- Resume game
      | SDL.keyboardEventKeyMotion evData == SDL.Pressed
          && ( (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeEscape
                 || (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeReturn && (ps ^. selectedOption) == ResumeGame
             ) ->
        do
          let (initCountdown, countSfx) = initialCountingDownState (assets ^. soundAssets) (ps ^. backgroundGS)
          applySideEffect countSfx
          return (CountingDown initCountdown, True)
      -- Restart game
      | SDL.keyboardEventKeyMotion evData == SDL.Pressed && (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeReturn && (ps ^. selectedOption == RestartGame) ->
        do
          let initGS = initialGameState (ps ^. backgroundGS . rand)
          let (initCountdown, countSfx) = initialCountingDownState (assets ^. soundAssets) initGS
          applySideEffect countSfx
          return (CountingDown initCountdown, True)
      -- Quit game
      | SDL.keyboardEventKeyMotion evData == SDL.Pressed && (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeReturn && (ps ^. selectedOption == PauseQuitGame) ->
        return (Paused ps, False)
      -- Select down
      | SDL.keyboardEventKeyMotion evData == SDL.Pressed && (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeDown ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . pauseSelectSfx))
          return (Paused (ps & selectedOption %~ selectDown), True)
      -- Select up
      | SDL.keyboardEventKeyMotion evData == SDL.Pressed && (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeUp ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . pauseSelectSfx))
          return (Paused (ps & selectedOption %~ selectUp), True)
      | otherwise -> return (Paused ps, True)
    _ -> return (Paused ps, True)

selectDown :: PauseOption -> PauseOption
selectDown ResumeGame = RestartGame
selectDown RestartGame = PauseQuitGame
selectDown PauseQuitGame = ResumeGame

selectUp :: PauseOption -> PauseOption
selectUp ResumeGame = PauseQuitGame
selectUp RestartGame = ResumeGame
selectUp PauseQuitGame = RestartGame

tickPaused :: Double -> PauseState -> IO MainStatePhase
tickPaused _ ps = return $ Paused ps

renderPaused :: SDL.Renderer -> Assets -> PauseState -> IO ()
renderPaused renderer assets ps =
  do
    renderGame renderer assets (ps ^. backgroundGS)
    renderTextCentered (assets ^. textAssets . font) renderer "PAUSED" (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 5)
    renderTextCentered (assets ^. textAssets . font) renderer resumeText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2 - 60)
    renderTextCentered (assets ^. textAssets . font) renderer restartText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2)
    renderTextCentered (assets ^. textAssets . font) renderer quitText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2 + 60)
  where
    resumeText = if ps ^. selectedOption == ResumeGame then "-- Resume game --" else "Resume game"
    restartText = if ps ^. selectedOption == RestartGame then "-- Restart game --" else "Restart game"
    quitText = if ps ^. selectedOption == PauseQuitGame then "-- Quit game --" else "Quit game"