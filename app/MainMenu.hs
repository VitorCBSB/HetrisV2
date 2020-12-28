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
import System.Random (mkStdGen, randomIO)
import Types
import UtilsSDL (renderTextCentered)

inputMainMenu :: SDL.EventPayload -> Assets -> MainMenuState -> IO (Maybe MainStatePhase)
inputMainMenu ev assets mms =
  case ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed repeat keySym)
      -- New marathon
      | SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption) == Marathon ->
        do
          rand <- randomIO
          let initGameState = initialGameState (mkStdGen rand)
          let (initCountdown, countSideEffect) = initialCountingDownState (assets ^. soundAssets) initGameState
          applySideEffect countSideEffect
          return (Just $ CountingDown initCountdown)
      -- Help screen
      | SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption == HelpOption) ->
        return (Just Help)
      -- Credits screen
      | SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption == CreditsOption) ->
        return (Just Credits)
      -- Quit game
      | SDL.keysymScancode keySym == SDL.ScancodeReturn && (mms ^. mainMenuSelectedOption == MainQuitGame) ->
        return Nothing
      -- Select down
      | SDL.keysymScancode keySym == SDL.ScancodeDown ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . menuSelectSfx))
          return (Just $ MainMenu (mms & mainMenuSelectedOption %~ selectDown))
      -- Select up
      | SDL.keysymScancode keySym == SDL.ScancodeUp ->
        do
          applySideEffect (PlayAudio (assets ^. soundAssets . menuSelectSfx))
          return (Just $ MainMenu (mms & mainMenuSelectedOption %~ selectUp))
      | otherwise -> return (Just $ MainMenu mms)
    _ -> return (Just $ MainMenu mms)

selectDown :: MainMenuOption -> MainMenuOption
selectDown Marathon = HelpOption
selectDown HelpOption = CreditsOption
selectDown CreditsOption = MainQuitGame
selectDown MainQuitGame = Marathon

selectUp :: MainMenuOption -> MainMenuOption
selectUp Marathon = MainQuitGame
selectUp HelpOption = Marathon
selectUp CreditsOption = HelpOption
selectUp MainQuitGame = CreditsOption

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
    helpText = if mms ^. mainMenuSelectedOption == HelpOption then "-- Help --" else "Help"
    creditsText = if mms ^. mainMenuSelectedOption == CreditsOption then "-- Credits --" else "Credits"
    quitText = if mms ^. mainMenuSelectedOption == MainQuitGame then "-- Quit --" else "Quit"
