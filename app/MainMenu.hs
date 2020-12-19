{-# LANGUAGE OverloadedStrings #-}

module MainMenu
  ( inputMainMenu,
    tickMainMenu,
    renderMainMenu,
  )
where

import Constants (windowSize)
import Control.Lens
import InitialStates (initialCountingDownState, initialGameState)
import qualified SDL
import System.Random (getStdGen)
import Types
import UtilsSDL (renderTextCentered)

inputMainMenu :: SDL.EventPayload -> Assets -> MainMenuState -> IO (MainStatePhase, Bool)
inputMainMenu ev assets mms =
  case ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ motion repeat keySym)
      -- New marathon
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption) == Marathon ->
        do
          initRand <- getStdGen
          let initGameState = initialGameState initRand
          let (initCountdown, countSideEffect) = initialCountingDownState (assets ^. soundAssets) initGameState
          applySideEffect countSideEffect
          return (CountingDown initCountdown, True)
      -- Help screen
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption == Help) ->
        return (MainMenu mms, True)
      -- Credits screen
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption == Credits) ->
        return (MainMenu mms, True)
      -- Quit game
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption == MainQuitGame) ->
        return (MainMenu mms, False)
      -- Select down
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeDown ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . menuSelectSfx))
          return (MainMenu (mms & mainMenuSelectedOption %~ selectDown), True)
      -- Select up
      | motion == SDL.Pressed && SDL.keysymScancode keySym == SDL.ScancodeUp ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . menuSelectSfx))
          return (MainMenu (mms & mainMenuSelectedOption %~ selectUp), True)
      | otherwise -> return (MainMenu mms, True)
    _ -> return (MainMenu mms, True)

selectDown :: MainMenuOption -> MainMenuOption
selectDown Marathon = Help
selectDown Help = Credits
selectDown Credits = MainQuitGame
selectDown MainQuitGame = Marathon

selectUp :: MainMenuOption -> MainMenuOption
selectUp Marathon = MainQuitGame
selectUp Help = Marathon
selectUp Credits = Help
selectUp MainQuitGame = Credits

tickMainMenu :: Double -> MainMenuState -> IO MainStatePhase
tickMainMenu _ mms = return (MainMenu mms)

renderMainMenu :: SDL.Renderer -> Assets -> MainMenuState -> IO ()
renderMainMenu renderer assets mms =
  do
    renderTextCentered (assets ^. textAssets . font) renderer "Hetris" (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 6)
    renderTextCentered (assets ^. textAssets . font) renderer marathonText (windowSize ^. _1 `div` 2, (windowSize ^. _2 `div` 3 * 2 - 48))
    renderTextCentered (assets ^. textAssets . font) renderer helpText (windowSize ^. _1 `div` 2, (windowSize ^. _2 `div` 3 * 2 - 24))
    renderTextCentered (assets ^. textAssets . font) renderer creditsText (windowSize ^. _1 `div` 2, (windowSize ^. _2 `div` 3 * 2))
    renderTextCentered (assets ^. textAssets . font) renderer quitText (windowSize ^. _1 `div` 2, (windowSize ^. _2 `div` 3 * 2 + 24))
  where
    marathonText = if mms ^. mainMenuSelectedOption == Marathon then "-- Marathon --" else "Marathon"
    helpText = if mms ^. mainMenuSelectedOption == Help then "-- Help --" else "Help"
    creditsText = if mms ^. mainMenuSelectedOption == Credits then "-- Credits --" else "Credits"
    quitText = if mms ^. mainMenuSelectedOption == MainQuitGame then "-- Quit --" else "Quit"
