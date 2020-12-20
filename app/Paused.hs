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
import InitialStates (initialCountingDownState, initialGameState, initialMainMenuState)
import qualified SDL
import Types
import UtilsSDL (renderTextCentered)

inputPaused :: SDL.EventPayload -> Assets -> PauseState -> IO (Maybe MainStatePhase)
inputPaused ev assets ps =
  case ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ motion repeat keySym)
      -- Resume game
      | motion == SDL.Pressed
          && ( SDL.keysymScancode keySym == SDL.ScancodeEscape
                 || SDL.keysymScancode keySym == SDL.ScancodeReturn && (ps ^. selectedOption) == ResumeGame
             ) ->
        do
          let (initCountdown, countSfx) = initialCountingDownState (assets ^. soundAssets) (ps ^. backgroundGS)
          applySideEffect countSfx
          return (Just $ CountingDown initCountdown)
      -- Restart game
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (ps ^. selectedOption == RestartGame) ->
        do
          let initGS = initialGameState (ps ^. backgroundGS . rand)
          let (initCountdown, countSfx) = initialCountingDownState (assets ^. soundAssets) initGS
          applySideEffect countSfx
          return (Just $ CountingDown initCountdown)
      -- Return to main menu
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (ps ^. selectedOption == BackToMainMenu) ->
        return (Just $ MainMenu initialMainMenuState)
      -- Quit game
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (ps ^. selectedOption == PauseQuitGame) ->
        return Nothing
      -- Select down
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeDown ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . menuSelectSfx))
          return (Just $ Paused (ps & selectedOption %~ selectDown))
      -- Select up
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeUp ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . menuSelectSfx))
          return (Just $ Paused (ps & selectedOption %~ selectUp))
      | otherwise -> return (Just $ Paused ps)
    _ -> return (Just $ Paused ps)

selectDown :: PauseOption -> PauseOption
selectDown ResumeGame = RestartGame
selectDown RestartGame = BackToMainMenu
selectDown BackToMainMenu = PauseQuitGame
selectDown PauseQuitGame = ResumeGame

selectUp :: PauseOption -> PauseOption
selectUp ResumeGame = PauseQuitGame
selectUp RestartGame = ResumeGame
selectUp BackToMainMenu = RestartGame
selectUp PauseQuitGame = BackToMainMenu

tickPaused :: Double -> PauseState -> IO MainStatePhase
tickPaused _ ps = return $ Paused ps

renderPaused :: SDL.Renderer -> Assets -> PauseState -> IO ()
renderPaused renderer assets ps =
  do
    renderGame renderer assets (ps ^. backgroundGS)
    renderTextCentered (assets ^. textAssets . font) renderer "PAUSED" (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 5)
    renderTextCentered (assets ^. textAssets . font) renderer resumeText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2 - 60)
    renderTextCentered (assets ^. textAssets . font) renderer restartText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2)
    renderTextCentered (assets ^. textAssets . font) renderer backToMainMenuText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2 + 60)
    renderTextCentered (assets ^. textAssets . font) renderer quitText (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2 + 120)
  where
    resumeText = if ps ^. selectedOption == ResumeGame then "-- Resume game --" else "Resume game"
    restartText = if ps ^. selectedOption == RestartGame then "-- Restart game --" else "Restart game"
    backToMainMenuText = if ps ^. selectedOption == BackToMainMenu then "-- Return to main menu --" else "Return to main menu"
    quitText = if ps ^. selectedOption == PauseQuitGame then "-- Quit game --" else "Quit game"