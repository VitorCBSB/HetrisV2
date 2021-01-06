{-# LANGUAGE OverloadedStrings #-}

module GameOver
  ( inputGameOver,
    tickGameOver,
    renderGameOver,
  )
where

import Constants (windowSize)
import Control.Lens
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Game (renderGame)
import InitialStates (initialMainMenuState)
import qualified SDL
import Types
import UtilsSDL (renderText, renderTextCentered)

finalCameraX :: Int
finalCameraX = 450

-- In seconds
cameraSlideTime :: Double
cameraSlideTime = 1

-- In seconds
gameOverTextTime :: Double
gameOverTextTime = 2

-- In seconds
statLineTimeInterval :: Double
statLineTimeInterval = 0.5

inputGameOver :: SDL.EventPayload -> Assets -> GameOverState -> IO MainStatePhase
inputGameOver ev assets gos =
  case ev of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ _) ->
      case gos ^. gameOverPhase of
        -- Pressing any key during the game over phase skips to the end of the little animation.
        -- Except if you are already at the end, in which case we just return to the main menu.
        Statistics _ l
          | l < length (statLines gos) -> return $ GameOver $ gos & gameOverPhase .~ Statistics 0 (length (statLines gos))
          | otherwise -> return $ MainMenu initialMainMenuState
        _ -> return $ GameOver $ gos & gameOverPhase .~ Statistics 0 (length (statLines gos))
    _ -> return $ GameOver gos

tickGameOver :: Double -> GameOverState -> IO MainStatePhase
tickGameOver dt gos =
  case gos ^. gameOverPhase of
    GameOverText t ->
      if t >= gameOverTextTime
        then return $ GameOver $ gos & gameOverPhase .~ Sliding 0
        else return $ GameOver $ gos & gameOverPhase .~ GameOverText (t + dt)
    Sliding t ->
      if t >= cameraSlideTime
        then return $ GameOver $ gos & gameOverPhase .~ Statistics 0 0
        else return $ GameOver $ gos & gameOverPhase .~ Sliding (t + dt)
    Statistics t l ->
      if l < length (statLines gos)
        then
          if t >= statLineTimeInterval
            then return $ GameOver $ gos & gameOverPhase .~ Statistics 0 (l + 1)
            else return $ GameOver $ gos & gameOverPhase .~ Statistics (t + dt) l
        else return $ GameOver $ gos & gameOverPhase .~ Statistics 0 l

statLines :: GameOverState -> [(T.Text, T.Text)]
statLines gos =
  let clearLines = map (\(l, c) -> (T.pack $ show l, T.pack $ show c)) (Map.toList (gos ^. finishedGame . gameStats . clears))
   in [ ("Pieces played", (T.pack . show) (gos ^. finishedGame . gameStats . piecesPlayed)),
        ("Max combo", (T.pack . show) (gos ^. finishedGame . gameStats . maxCombo)),
        ("Lines cleared", (T.pack . show) (gos ^. finishedGame . linesCleared))
      ]
        <> clearLines

renderGameOver :: SDL.Renderer -> Assets -> GameOverState -> IO ()
renderGameOver renderer assets gos =
  do
    renderGame renderer assets (gos ^. finishedGame) cameraPos
    case gos ^. gameOverPhase of
      GameOverText _ ->
        do
          renderTextCentered (assets ^. textAssets . font) renderer "GAME OVER" (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2)
      Statistics _ lines ->
        do
          renderTextCentered (assets ^. textAssets . font) renderer "Stats" (750, 100)
          forM_ (take lines (zip [0 ..] (statLines gos))) $ \(i, (l, c)) ->
            do
              renderText (assets ^. textAssets . font) renderer l (550, 200 + i * 40)
              renderText (assets ^. textAssets . font) renderer c (900, 200 + i * 40)
      _ -> return ()
  where
    cameraPos =
      case gos ^. gameOverPhase of
        GameOverText _ -> (0, 0)
        Statistics _ _ -> (finalCameraX, 0)
        Sliding t -> (floor $ (fromIntegral finalCameraX) * t / cameraSlideTime, 0)