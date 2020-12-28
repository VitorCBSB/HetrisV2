{-# LANGUAGE OverloadedStrings #-}

module Help
  ( inputHelp,
    tickHelp,
    renderHelp,
  )
where

import Constants (windowSize)
import Control.Lens
import Control.Monad (forM_)
import InitialStates (initialMainMenuState)
import qualified SDL
import Types
import UtilsSDL (renderText, renderTextCentered)

inputHelp :: SDL.EventPayload -> IO MainStatePhase
inputHelp event =
  case event of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ _) ->
      return $ MainMenu initialMainMenuState
    _ -> return Help

tickHelp :: Double -> IO MainStatePhase
tickHelp _ = return Help

renderHelp :: SDL.Renderer -> Assets -> IO ()
renderHelp renderer assets =
  do
    renderTextCentered (assets ^. textAssets . font) renderer "Controls" ((windowSize ^. _1) `div` 2, titleY)
    forM_ (zip [0 ..] controls) $ \(i, (c, k)) ->
      do
        renderText (assets ^. textAssets . font) renderer c (leftX, baseY + (i * 36))
        renderText (assets ^. textAssets . font) renderer k (rightX, baseY + (i * 36))
    renderTextCentered (assets ^. textAssets . font) renderer "Press any key to return to the main menu." ((windowSize ^. _1) `div` 2, 700)
  where
    titleY = 50
    baseY = 200
    leftX = 180
    rightX = 550
    controls =
      [ ("Rotate right", "Up arrow or X"),
        ("Hard drop", "Space"),
        ("Hold", "Left Shift or C"),
        ("Rotate left", "Ctrl or Z"),
        ("Pause", "Esc or F1"),
        ("Movement", "Left and right arrows"),
        ("Soft drop", "Down arrow"),
        ("Lock delay cancel", "Down arrow")
      ]