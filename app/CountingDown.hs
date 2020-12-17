module CountingDown
  ( inputCountingDown,
    tickCountingDown,
    renderCountingDown,
  )
where

import Constants (windowSize)
import Control.Lens
import qualified Data.Text as T
import Game (renderGame)
import InitialStates (initialPauseState)
import qualified SDL
import Types
import UtilsSDL (renderTextCentered)

inputCountingDown :: SDL.EventPayload -> Assets -> CountingDownState -> IO MainStatePhase
inputCountingDown ev gameAssets cds =
  case ev of
    SDL.KeyboardEvent evData ->
      if SDL.keyboardEventKeyMotion evData == SDL.Pressed && ((SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeEscape || (SDL.keysymScancode . SDL.keyboardEventKeysym) evData == SDL.ScancodeF1)
        then do
          let (initPauseState, pauseSfx) = initialPauseState (gameAssets ^. soundAssets) (cds ^. gameStateToReturnTo)
          applySideEffect pauseSfx
          return $ Paused initPauseState
        else return $ CountingDown cds
    _ -> return (CountingDown cds)

tickCountingDown :: Double -> Assets -> CountingDownState -> IO MainStatePhase
tickCountingDown dt assets cds =
  if cds ^. countdownTime >= 1.0
    then
      if cds ^. countdown <= 1
        then do
          applySideEffect (PlayAudio (assets ^. soundAssets . countdownGoSfx))
          return $ Game (cds ^. gameStateToReturnTo)
        else do
          applySideEffect (PlayAudio (assets ^. soundAssets . countdownSfx))
          return $ CountingDown $ cds & countdown -~ 1 & countdownTime -~ 1
    else return $ CountingDown $ cds & countdownTime +~ dt

renderCountingDown :: SDL.Renderer -> Assets -> CountingDownState -> IO ()
renderCountingDown renderer assets cds =
  do
    renderGame renderer assets (cds ^. gameStateToReturnTo)
    renderTextCentered (assets ^. textAssets . font) renderer (T.pack $ show $ cds ^. countdown) (windowSize ^. _1 `div` 2, windowSize ^. _2 `div` 2)