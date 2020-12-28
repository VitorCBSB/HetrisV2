{-# LANGUAGE OverloadedStrings #-}

module Credits
  ( inputCredits,
    tickCredits,
    renderCredits,
  )
where

import Constants (windowSize)
import Control.Lens
import InitialStates (initialMainMenuState)
import qualified SDL
import Types
import UtilsSDL (renderTextCentered)

inputCredits :: SDL.EventPayload -> IO MainStatePhase
inputCredits event =
  case event of
    SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ _) ->
      return $ MainMenu initialMainMenuState
    _ -> return Credits

tickCredits :: Double -> IO MainStatePhase
tickCredits _ = return Credits

renderCredits :: SDL.Renderer -> Assets -> IO ()
renderCredits renderer assets =
  do
    renderTextCentered (assets ^. textAssets . font) renderer "Hetris" ((windowSize ^. _1) `div` 2, baseY)
    renderTextCentered (assets ^. textAssets . font) renderer "Programming" ((windowSize ^. _1) `div` 2, baseY + 124)
    renderTextCentered (assets ^. textAssets . font) renderer "Graphics (lol)" ((windowSize ^. _1) `div` 2, baseY + 148)
    renderTextCentered (assets ^. textAssets . font) renderer "Sound Effects" ((windowSize ^. _1) `div` 2, baseY + 172)
    renderTextCentered (assets ^. textAssets . font) renderer "Vitor Coimbra" ((windowSize ^. _1) `div` 2, baseY + 220)
    renderTextCentered (assets ^. textAssets . font) renderer "Special thanks to" ((windowSize ^. _1) `div` 2, baseY + 300)
    renderTextCentered (assets ^. textAssets . font) renderer "Luis Fernando (Wizky)" ((windowSize ^. _1) `div` 2, baseY + 360)
    renderTextCentered (assets ^. textAssets . font) renderer "The /r/haskell subreddit" ((windowSize ^. _1) `div` 2, baseY + 384)
    renderTextCentered (assets ^. textAssets . font) renderer "The /r/haskellgamedev subreddit" ((windowSize ^. _1) `div` 2, baseY + 408)
    renderTextCentered (assets ^. textAssets . font) renderer "You :)" ((windowSize ^. _1) `div` 2, baseY + 432)
    renderTextCentered (assets ^. textAssets . font) renderer "Press any key to return to the main menu." ((windowSize ^. _1) `div` 2, baseY + 650)
  where
    baseY = 50