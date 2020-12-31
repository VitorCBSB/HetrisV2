module InitialStates
  ( initialCountingDownState,
    initialPauseState,
    initialGameState,
    initialIntroState,
    initialMainMenuState,
    initialGameOverState,
  )
where

import Constants (fieldHeight, fieldWidth)
import Control.Lens
import Data.Array
import System.Random (StdGen)
import Types

initialIntroState :: IntroState
initialIntroState = IntroState 0

initialMainMenuState :: MainMenuState
initialMainMenuState = MainMenuState Endless

initialCountingDownState :: SoundAssets -> GameState -> Bool -> (CountingDownState, SideEffect)
initialCountingDownState soundAssets gs fromPause =
  (CountingDownState gs 3 0 fromPause, PlayAudio (soundAssets ^. countdownSfx))

initialPauseState :: SoundAssets -> GameState -> (PauseState, [SideEffect])
initialPauseState soundAssets gs =
  (PauseState gs ResumeGame, [PlayAudio (soundAssets ^. pauseSfx), PauseMusic])

initialGameOverState :: SoundAssets -> GameState -> (GameOverState, [SideEffect])
initialGameOverState soundAssets gs =
  (GameOverState gs (GameOverText 0), [PlayAudio (soundAssets ^. defeatSfx), HaltMusic])

initialGameState :: StdGen -> GameState
initialGameState initRand =
  GameState cleanBoard initNextShapes initNextBag Nothing (Placing newPlacingState) 0 Nothing False 0 0 rand'' initTexts
  where
    cleanBoard =
      array
        (0, fieldHeight - 1)
        [(i, array (0, fieldWidth - 1) [(j, Nothing) | j <- [0 .. fieldWidth - 1]]) | i <- [0 .. fieldHeight - 1]]
    debugPerfectClearBoard =
      cleanBoard
        // [ (0, array (0, fieldWidth - 1) ([(0, Just S), (1, Nothing)] <> map (\j -> (j, Just S)) [2 .. fieldWidth - 1])),
             (1, array (0, fieldWidth - 1) ([(0, Nothing), (1, Nothing), (2, Nothing)] <> map (\j -> (j, Just S)) [3 .. fieldWidth - 1]))
           ]
    debugDTCannonBoard =
      cleanBoard
        // [ (0, array (0, fieldWidth - 1) ([(2, Nothing)] <> map (\j -> (j, Just S)) [0, 1, 3, 4, 5, 6, 7, 8, 9])),
             (1, array (0, fieldWidth - 1) ([(2, Nothing)] <> map (\j -> (j, Just S)) [0, 1, 3, 4, 5, 6, 7, 8, 9])),
             (2, array (0, fieldWidth - 1) ([(1, Nothing), (2, Nothing), (3, Nothing)] <> map (\j -> (j, Just S)) [0, 4, 5, 6, 7, 8, 9])),
             (3, array (0, fieldWidth - 1) ([(1, Nothing), (2, Nothing)] <> map (\j -> (j, Just S)) [0, 3, 4, 5, 6, 7, 8, 9])),
             (4, array (0, fieldWidth - 1) ([(2, Nothing)] <> map (\j -> (j, Just S)) [0, 1, 3, 4, 5, 6, 7, 8, 9])),
             (5, array (0, fieldWidth - 1) ([(3, Just S)] <> map (\j -> (j, Nothing)) [0, 1, 2, 4, 5, 6, 7, 8, 9])),
             (6, array (0, fieldWidth - 1) ([(2, Just S), (3, Just S)] <> map (\j -> (j, Nothing)) [0, 1, 4, 5, 6, 7, 8, 9]))
           ]
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