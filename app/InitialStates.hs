module InitialStates
  ( initialCountingDownState,
    initialPauseState,
    initialGameState,
  )
where
import Constants
import Control.Lens
import Data.Array
import System.Random
import Types

initialCountingDownState :: SoundAssets -> GameState -> (CountingDownState, SideEffect)
initialCountingDownState soundAssets gs =
  (CountingDownState gs 3 0, PlayAudio (soundAssets ^. countdownSfx))

initialPauseState :: SoundAssets -> GameState -> (PauseState, SideEffect)
initialPauseState soundAssets gs =
  (PauseState gs ResumeGame, PlayAudio (soundAssets ^. pauseSfx))

initialGameState :: StdGen -> GameState
initialGameState initRand =
  GameState cleanBoard initNextShapes initNextBag Nothing (Placing newPlacingState) 0 Nothing False 0 0 rand'' initTexts
  where
    cleanBoard =
      array
        (0, fieldHeight - 1)
        [(i, array (0, fieldWidth - 1) [(j, Nothing) | j <- [0 .. fieldWidth - 1]]) | i <- [0 .. fieldHeight - 1]]
    ((initTetShape, next1, next2, next3, next4, next5, next6), rand') = newTetrominoBag initRand
    ((bagNext1, bagNext2, bagNext3, bagNext4, bagNext5, bagNext6, bagNext7), rand'') = newTetrominoBag rand'
    initNextBag = (Just bagNext1, Just bagNext2, Just bagNext3, Just bagNext4, Just bagNext5, Just bagNext6, Just bagNext7)
    initTet = Tetromino (20.5, 4) initTetShape Base
    initTexts = []
    newPlacingState =
      PlacingState
        { _tetromino = initTet,
          _fromHeld = False,
          _lockingTime = Nothing,
          _softDropScore = 0,
          _hardDropScore = 0
        }
    initNextShapes = (next1, next2, next3, next4, next5, next6)