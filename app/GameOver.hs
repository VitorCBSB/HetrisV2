{-# LANGUAGE OverloadedStrings #-}

module GameOver
  ( inputGameOver
  , tickGameOver
  , renderGameOver
  )
  where

import Constants
import Control.Lens
import Game (renderGame)
import InitialStates
import Types
import qualified SDL
import UtilsSDL

inputGameOver :: SDL.EventPayload -> Assets -> GameOverState -> IO MainStatePhase
inputGameOver ev assets gos = 
  case ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ _) ->
      return $ MainMenu initialMainMenuState 
    _ -> return $ GameOver gos

tickGameOver :: Double -> GameOverState -> IO MainStatePhase
tickGameOver dt gos = return $ GameOver gos

renderGameOver :: SDL.Renderer -> Assets -> GameOverState -> IO ()
renderGameOver renderer assets gos = 
  case gos ^. gameOverPhase of
    GameOverText _ -> 
      do
        renderGame renderer assets (gos ^. finishedGame)
        renderTextCentered (assets ^. textAssets . font) renderer "GAME OVER" (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2)
    _ -> return ()