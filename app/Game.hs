{-# LANGUAGE OverloadedStrings #-}

module Game
  ( inputGame,
    tickGame,
    renderGame,
  )
where

import Constants
import Control.Lens
import Control.Monad (forM_, when)
import Data.Array
import Data.List (find, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Text as T
import FloatingText (FloatingText, makeFloatingText, renderFloatingText, tickFloatingText)
import InitialStates (initialGameOverState, initialPauseState)
import Linear.V4 (V4 (V4))
import qualified SDL
import qualified SDL.Font as Ttf
import qualified SDL.Mixer as Mixer
import System.Random (RandomGen, StdGen)
import Types
import Utils (fisherYatesShuffle, tupleHistogram)
import UtilsSDL (renderTextCentered, renderTexture, renderTextureCentered)

-- In cells per second
normalDropSpeed :: TetrisLevel -> Double
normalDropSpeed (TetrisLevel l) = 1 / ((0.8 - fromIntegral l * 0.007) ^ fromIntegral l)

inputGame :: SDL.EventPayload -> Assets -> GameState -> IO MainStatePhase
inputGame ev assets gs =
  do
    let (newMSP, sideEffs) = inputGame' ev assets gs
    mapM_ applySideEffect sideEffs
    return newMSP

inputGame' :: SDL.EventPayload -> Assets -> GameState -> (MainStatePhase, [SideEffect])
inputGame' ev assets gs =
  case ev of
    SDL.KeyboardEvent evData
      | SDL.keyboardEventKeyMotion evData == SDL.Pressed ->
        inputPress evData assets gs
      | SDL.keyboardEventKeyMotion evData == SDL.Released ->
        inputRelease evData gs
      | otherwise -> (Game gs, [])
    _ -> (Game gs, [])

inputPress :: SDL.KeyboardEventData -> Assets -> GameState -> (MainStatePhase, [SideEffect])
inputPress (SDL.KeyboardEventData _ _ repeat keySym) assets gs =
  case gs ^. phase of
    Placing placingState
      -- Counter clockwise rotation
      | SDL.keysymScancode keySym == SDL.ScancodeZ || SDL.keysymScancode keySym == SDL.ScancodeLCtrl ->
        attemptCounterClockwiseRotation placingState assets gs & _1 %~ Game
      -- Clockwise rotation
      | SDL.keysymScancode keySym == SDL.ScancodeX || SDL.keysymScancode keySym == SDL.ScancodeUp ->
        attemptClockwiseRotation placingState assets gs & _1 %~ Game
      -- Move left
      | SDL.keysymScancode keySym == SDL.ScancodeLeft ->
        attemptToMoveLeft placingState assets gs & _1 %~ Game
      -- Move right
      | SDL.keysymScancode keySym == SDL.ScancodeRight ->
        attemptToMoveRight placingState assets gs & _1 %~ Game
      -- Soft drop / lock piece if locking
      | SDL.keysymScancode keySym == SDL.ScancodeDown && not repeat ->
        case placingState ^. lockingTime of
          Nothing -> (Game $ gs & wantsToSoftDrop .~ True, [])
          Just _ -> lockPiece placingState assets gs
      -- Hold piece
      | (SDL.keysymScancode keySym == SDL.ScancodeC || SDL.keysymScancode keySym == SDL.ScancodeLShift) && not repeat && not (placingState ^. fromHeld) ->
        case gs ^. heldPiece of
          Nothing ->
            let newGS = gs & heldPiece ?~ placingState ^. tetromino . shape
             in sendNextTetromino True assets newGS
          Just ts ->
            let newGS = gs & heldPiece ?~ placingState ^. tetromino . shape
             in sendTetromino ts True assets newGS
      -- Hard drop
      | SDL.keysymScancode keySym == SDL.ScancodeSpace && not repeat && isNothing (placingState ^. lockingTime) ->
        hardDrop placingState assets gs & _1 %~ Game
      | (SDL.keysymScancode keySym == SDL.ScancodeEscape || SDL.keysymScancode keySym == SDL.ScancodeF1) && not repeat ->
        let newGS = gs & wantsToSoftDrop .~ False
            (pauseState, pauseSfx) = initialPauseState (assets ^. soundAssets) newGS
         in (Paused pauseState, pauseSfx)
      | otherwise -> (Game gs, [])
    ClearingLines _ ->
      -- Pause
      if (SDL.keysymScancode keySym == SDL.ScancodeEscape || SDL.keysymScancode keySym == SDL.ScancodeF1)
        && not repeat
        then
          let newGS = gs & wantsToSoftDrop .~ False
              (pauseState, pauseSfx) = initialPauseState (assets ^. soundAssets) newGS
           in (Paused pauseState, pauseSfx)
        else (Game gs, [])

inputRelease :: SDL.KeyboardEventData -> GameState -> (MainStatePhase, [SideEffect])
inputRelease (SDL.KeyboardEventData _ _ _ keySym) gs =
  case gs ^. phase of
    Placing _ ->
      if SDL.keysymScancode keySym == SDL.ScancodeDown
        then (Game $ gs & wantsToSoftDrop .~ False, [])
        else (Game gs, [])
    _ -> (Game gs, [])

hardDrop :: PlacingState -> Assets -> GameState -> (GameState, [SideEffect])
hardDrop ps assets gs =
  let newTet = dropTetromino (gs ^. board) (ps ^. tetromino)
      dropDiff = floor (ps ^. tetromino . pos . _1) - floor (newTet ^. pos . _1)
      scoreToAward = min (40 - ps ^. hardDropScore) (dropDiff * 2)
   in ( gs
          & phase
            .~ Placing
              ( ps & tetromino .~ newTet
                  & lockingTime ?~ 0
                  & hardDropScore +~ scoreToAward
              )
          & score +~ scoreToAward,
        [PlayAudio (assets ^. soundAssets . landOnSurfaceSfx)]
      )

attemptToMoveLeft :: PlacingState -> Assets -> GameState -> (GameState, [SideEffect])
attemptToMoveLeft ps assets gs =
  let (movedTetromino, successful) = shiftToLeft (gs ^. board) (ps ^. tetromino)
      (_, successfulDown) = shiftTetrominoDown (gs ^. board) movedTetromino
      (_, successfulLeft) = shiftToLeft (gs ^. board) movedTetromino
      wasLocking = isJust $ ps ^. lockingTime
      sideEffects =
        if successful
          then
            if successfulLeft
              then [PlayAudio (assets ^. soundAssets . movementSfx)]
              else [PlayAudio (assets ^. soundAssets . hitWallSfx)]
          else []
   in -- A successful shift resets locking time
      ( if successful
          then
            if successfulDown
              then
                gs & phase
                  .~ Placing
                    ( ps & tetromino .~ movedTetromino
                        -- Also reset our position's decimal point to 0.5
                        -- if we were locking
                        & (tetromino . pos . _1) %~ (\i -> if wasLocking then fromIntegral (floor i) + 0.5 else i)
                        & lockingTime .~ Nothing
                    )
              else gs & phase .~ Placing (ps & tetromino .~ movedTetromino & lockingTime ?~ 0)
          else gs & phase .~ Placing (ps & tetromino .~ movedTetromino),
        sideEffects
      )

shiftToLeft :: Board -> Tetromino -> (Tetromino, Bool)
shiftToLeft board tetromino =
  let shiftedTetromino = tetromino & (pos . _2) %~ (\j -> j - 1)
      shiftedIsObstructed = isTetrominoObstructed board shiftedTetromino
      newTetromino = if shiftedIsObstructed then tetromino else shiftedTetromino
   in (newTetromino, not shiftedIsObstructed)

attemptToMoveRight :: PlacingState -> Assets -> GameState -> (GameState, [SideEffect])
attemptToMoveRight ps assets gs =
  let (movedTetromino, successful) = shiftToRight (gs ^. board) (ps ^. tetromino)
      (_, successfulDown) = shiftTetrominoDown (gs ^. board) movedTetromino
      (_, successfulRight) = shiftToRight (gs ^. board) movedTetromino
      wasLocking = isJust $ ps ^. lockingTime
      sideEffects =
        if successful
          then
            if successfulRight
              then [PlayAudio (assets ^. soundAssets . movementSfx)]
              else [PlayAudio (assets ^. soundAssets . hitWallSfx)]
          else []
   in -- A successful shift resets locking time
      ( if successful
          then
            if successfulDown
              then
                gs & phase
                  .~ Placing
                    ( ps & tetromino .~ movedTetromino
                        -- Also reset our position's decimal point to 0.5
                        -- if we were locking
                        & (tetromino . pos . _1) %~ (\i -> if wasLocking then fromIntegral (floor i) + 0.5 else i)
                        & lockingTime .~ Nothing
                    )
              else gs & phase .~ Placing (ps & tetromino .~ movedTetromino & lockingTime ?~ 0)
          else gs & phase .~ Placing (ps & tetromino .~ movedTetromino),
        sideEffects
      )

shiftToRight :: Board -> Tetromino -> (Tetromino, Bool)
shiftToRight board tetromino =
  let shiftedTetromino = tetromino & (pos . _2) %~ (+ 1)
      shiftedIsObstructed = isTetrominoObstructed board shiftedTetromino
      newTetromino = if shiftedIsObstructed then tetromino else shiftedTetromino
   in (newTetromino, not shiftedIsObstructed)

clockwiseRotationTranslationsToAttempt :: TetrominoShape -> TetrominoRotation -> [(Int, Int)]
clockwiseRotationTranslationsToAttempt shape rot =
  case shape of
    O -> [(0, 0)]
    I -> case rot of
      Base -> [(0, 0), (0, -2), (0, 1), (-1, -2), (2, 1)]
      Ninety -> [(0, 0), (0, -1), (0, 2), (2, -1), (-1, 2)]
      OneEighty -> [(0, 0), (0, 2), (0, -1), (1, 2), (-2, -1)]
      TwoSeventy -> [(0, 0), (0, 1), (0, -2), (-2, 1), (1, -2)]
    _ -> case rot of
      Base -> [(0, 0), (0, -1), (1, -1), (-2, 0), (-2, -1)]
      Ninety -> [(0, 0), (0, 1), (-1, 1), (2, 0), (2, 1)]
      OneEighty -> [(0, 0), (0, 1), (1, 1), (-2, 0), (-2, 1)]
      TwoSeventy -> [(0, 0), (0, -1), (-1, -1), (2, 0), (2, -1)]

counterClockwiseRotationTranslationsToAttempt :: TetrominoShape -> TetrominoRotation -> [(Int, Int)]
counterClockwiseRotationTranslationsToAttempt shape rot =
  case shape of
    O -> [(0, 0)]
    I -> case rot of
      Base -> [(0, 0), (0, -1), (0, 2), (2, -1), (-1, 2)]
      Ninety -> [(0, 0), (0, 2), (-1, 0), (2, 1), (-1, -2)]
      OneEighty -> [(0, 0), (0, 1), (0, -2), (-2, 1), (1, -2)]
      TwoSeventy -> [(0, 0), (0, -2), (0, 1), (-1, -2), (2, 1)]
    _ -> case rot of
      Base -> [(0, 0), (0, 1), (1, 1), (-2, 0), (-2, 1)]
      Ninety -> [(0, 0), (0, 1), (-1, 1), (2, 0), (2, 1)]
      OneEighty -> [(0, 0), (0, -1), (1, -1), (-2, 0), (-2, -1)]
      TwoSeventy -> [(0, 0), (0, -1), (-1, -1), (2, 0), (2, -1)]

attemptRotation :: (Board -> Tetromino -> (Tetromino, Bool)) -> PlacingState -> Assets -> GameState -> (GameState, [SideEffect])
attemptRotation rotationF ps assets gs =
  let (newTet, successful) = rotationF (gs ^. board) (ps ^. tetromino)
      (_, successfulDown) = shiftTetrominoDown (gs ^. board) newTet
   in if successful
        then
          let sideEffects =
                [PlayAudio (assets ^. soundAssets . rotationSfx)]
                  <> if isNothing (ps ^. lockingTime) && (not successfulDown) then [PlayAudio (assets ^. soundAssets . landOnSurfaceSfx)] else []
              newLockingTime = if successfulDown then Nothing else Just 0
           in (gs & phase .~ Placing (ps & tetromino .~ newTet & lockingTime .~ newLockingTime), sideEffects)
        else (gs, [])

attemptClockwiseRotation :: PlacingState -> Assets -> GameState -> (GameState, [SideEffect])
attemptClockwiseRotation =
  attemptRotation clockwiseRotation

clockwiseRotation :: Board -> Tetromino -> (Tetromino, Bool)
clockwiseRotation board tetromino =
  let rotatedTetromino = tetromino & rotation %~ tetrominoClockwiseRotation
      translations = clockwiseRotationTranslationsToAttempt (tetromino ^. shape) (tetromino ^. rotation)
      (tetI, tetJ) = tetromino ^. pos
      maybeSuccessfullyRotatedTetromino =
        find (not . isTetrominoObstructed board) $ map (\(i, j) -> rotatedTetromino & pos .~ (tetI + fromIntegral i, tetJ + j)) translations
   in case maybeSuccessfullyRotatedTetromino of
        Just newTet -> (newTet, True)
        Nothing -> (tetromino, False)

attemptCounterClockwiseRotation :: PlacingState -> Assets -> GameState -> (GameState, [SideEffect])
attemptCounterClockwiseRotation =
  attemptRotation counterClockwiseRotation

counterClockwiseRotation :: Board -> Tetromino -> (Tetromino, Bool)
counterClockwiseRotation board tetromino =
  let rotatedTetromino = tetromino & rotation %~ tetrominoCounterclockwiseRotation
      translations = counterClockwiseRotationTranslationsToAttempt (tetromino ^. shape) (tetromino ^. rotation)
      (tetI, tetJ) = tetromino ^. pos
      maybeSuccessfullyRotatedTetromino =
        find (not . isTetrominoObstructed board) $ map (\(i, j) -> rotatedTetromino & pos .~ (tetI + fromIntegral i, tetJ + j)) translations
   in case maybeSuccessfullyRotatedTetromino of
        Just newTet -> (newTet, True)
        Nothing -> (tetromino, False)

tickGame :: Double -> Assets -> GameState -> IO MainStatePhase
tickGame dt assets gs =
  do
    let (newMS, sideEffs) = tickGame' dt assets gs
    mapM_ applySideEffect sideEffs
    return newMS

tickGame' :: Double -> Assets -> GameState -> (MainStatePhase, [SideEffect])
tickGame' dt assets gs =
  let postTextsGs =
        gs
          & floatingTexts .~ mapMaybe (tickFloatingText dt) (gs ^. floatingTexts)
          & elapsedTime +~ dt
   in case postTextsGs ^. phase of
        Placing placingState -> tickPlacingState placingState dt assets postTextsGs
        ClearingLines t ->
          if t >= clearingLinesTime
            then
              let newBoard = foldl' dropLine (postTextsGs ^. board) [0 .. fieldHeight - 1]
                  (newPhase, sideEffects) = sendNextTetromino False assets (postTextsGs {_board = newBoard})
                  sideEffects' = sideEffects <> if newBoard == (postTextsGs ^. board) then [] else [PlayAudio (assets ^. soundAssets . lineDropSfx)]
               in (newPhase, sideEffects')
            else (Game $ postTextsGs & phase .~ ClearingLines (t + dt), [])

dropLine :: Board -> Int -> Board
dropLine board row =
  let emptyLine line = and (isNothing <$> line)
      emptyLineToPlace = (+ 1) $ fromJust $ find (\l -> l < 0 || not (emptyLine (board ! l))) [row - 1, row - 2 ..]
   in if emptyLineToPlace == row
        then board
        else board // [(emptyLineToPlace, board ! row), (row, array (0, fieldWidth - 1) [(c, Nothing) | c <- [0 .. fieldWidth - 1]])]

isTetrominoObstructed :: Board -> Tetromino -> Bool
isTetrominoObstructed board tetromino =
  let (bl1, bl2, bl3, bl4) = relativeBlocksFromRotation (tetromino ^. shape) (tetromino ^. rotation)
      (tetI, tetJ) = tetromino ^. pos
      blocks = map (\(i, j) -> (floor tetI + i, tetJ + j)) [bl1, bl2, bl3, bl4]
   in any (\(blI, blJ) -> blI < 0 || blJ < 0 || blJ >= fieldWidth || isJust (board ! blI ! blJ)) blocks

dropTetromino :: Board -> Tetromino -> Tetromino
dropTetromino board tetromino =
  let moveTetromino = shiftTetrominoDown board
      (droppedTetromino, _) = until (\(_, s) -> not s) (\(t, _) -> moveTetromino t) (tetromino, True)
   in droppedTetromino

-- Returns the new, maybe shifted down, tetromino.
-- Also returns if the move was succesful or if we were obstructed.
shiftTetrominoUp :: Board -> Tetromino -> (Tetromino, Bool)
shiftTetrominoUp board tetromino =
  let shiftedTetromino = tetromino & pos . _1 %~ (+ 1)
      shiftedIsObstructed = isTetrominoObstructed board shiftedTetromino
      newTetromino = if shiftedIsObstructed then tetromino else shiftedTetromino
   in (newTetromino, not shiftedIsObstructed)

-- Returns the new, maybe shifted down, tetromino.
-- Also returns if the move was succesful or if we were obstructed.
shiftTetrominoDown :: Board -> Tetromino -> (Tetromino, Bool)
shiftTetrominoDown board tetromino =
  let shiftedTetromino = tetromino & pos . _1 %~ (\i -> i - 1)
      shiftedIsObstructed = isTetrominoObstructed board shiftedTetromino
      newTetromino = if shiftedIsObstructed then tetromino else shiftedTetromino
   in (newTetromino, not shiftedIsObstructed)

-- Send the argument shape
sendTetromino :: TetrominoShape -> Bool -> Assets -> GameState -> (MainStatePhase, [SideEffect])
sendTetromino shape fromHeld assets gs =
  let newTetromino = Tetromino (20.5, 4) shape Base
      defeated = isTetrominoObstructed (gs ^. board) newTetromino
      newLockingTime =
        -- Did we spawn already touching something? Then start locking right away.
        if snd $ shiftTetrominoDown (gs ^. board) newTetromino
          then Nothing
          else Just 0
      newPhase =
        Placing (PlacingState newTetromino fromHeld newLockingTime 0 0)
   in if defeated
        then
          let (initGameOver, sideEffs) = initialGameOverState (assets ^. soundAssets) gs
           in (GameOver initGameOver, sideEffs)
        else (Game $ gs & phase .~ newPhase, [])

-- Spawn the next piece to be sent. Returns MainStatePhase because this can make us lose game
-- if a piece spawns overlapping a block.
sendNextTetromino :: Bool -> Assets -> GameState -> (MainStatePhase, [SideEffect])
sendNextTetromino fromHeld assets gs =
  -- Generate a new bag if the one we have has been emptied
  let (Just ns6, mns7, mns8, mns9, mns10, mns11, mns12) = (gs ^. nextTetrominoBag)
      (newBag, newRand) =
        case mns7 of
          Nothing ->
            let ((nns6, nns7, nns8, nns9, nns10, nns11, nns12), rand') = newTetrominoBag (gs ^. rand)
             in ((Just nns6, Just nns7, Just nns8, Just nns9, Just nns10, Just nns11, Just nns12), rand')
          Just _ -> ((mns7, mns8, mns9, mns10, mns11, mns12, Nothing), gs ^. rand)
      (ns0, ns1, ns2, ns3, ns4, ns5) = gs ^. nextShapes
      newNextShapes = (ns1, ns2, ns3, ns4, ns5, ns6)
      newGS =
        gs & rand .~ newRand
          & nextTetrominoBag .~ newBag
          & nextShapes .~ newNextShapes
          & (gameStats . piecesPlayed) %~ (+ 1)
   in sendTetromino ns0 fromHeld assets newGS

levelFromLinesCleared :: Int -> TetrisLevel
levelFromLinesCleared linesCleared = TetrisLevel $ linesCleared `div` linesLevelUp

calculateLockType :: Tetromino -> Board -> LockType
calculateLockType t b =
  let (successLeft, successRight, successUp) =
        ( snd $ shiftToLeft b t,
          snd $ shiftToRight b t,
          snd $ shiftTetrominoUp b t
        )
      tspin = (t ^. shape) == T && not successLeft && not successRight && not successUp
      tHeadAdjacents =
        case t ^. rotation of
          Base -> [(1, -1), (1, 1)]
          Ninety -> [(1, 1), (-1, 1)]
          OneEighty -> [(-1, -1), (-1, 1)]
          TwoSeventy -> [(-1, -1), (1, -1)]
      (tetI, tetJ) = t ^. pos
      tileIsFilled (i, j) =
        let (bI, bJ) = (floor $ tetI + i, tetJ + j)
         in bI < 0 || bJ < 0 || bJ >= fieldWidth || isJust (b ! bI ! bJ)
      headNeighborsAreFilled = all tileIsFilled tHeadAdjacents
      miniTspin = not headNeighborsAreFilled
   in if tspin
        then
          if miniTspin
            then MiniTSpinLock
            else TSpinLock
        else NormalLock

calculateClearType :: Int -> Maybe LineClear -> Tetromino -> Board -> LineClear
calculateClearType linesToClear maybeLastClear t b =
  let lockType = calculateLockType t b
      difficultClear c = c /= Single && c /= Double && c /= Triple
   in case linesToClear of
        1 ->
          case lockType of
            NormalLock -> Single
            MiniTSpinLock -> MiniTSpinSingle
            TSpinLock ->
              case maybeLastClear of
                Nothing -> TSpinSingle
                Just c -> if difficultClear c then B2BTSpinSingle else TSpinSingle
        2 ->
          if lockType == TSpinLock
            then case maybeLastClear of
              Nothing -> TSpinDouble
              Just c -> if difficultClear c then B2BTSpinDouble else TSpinDouble
            else Double
        3 ->
          if lockType == TSpinLock
            then case maybeLastClear of
              Nothing -> TSpinTriple
              Just c -> if difficultClear c then B2BTSpinTriple else TSpinTriple
            else Triple
        4 ->
          case maybeLastClear of
            Nothing -> Tetris
            Just c -> if difficultClear c then B2BTetris else Tetris
        _ -> error "Lines to clear was either greater than 4 or less than 0. Basically impossible to happen."

scoreFromLineClear :: LineClear -> TetrisLevel -> Int
scoreFromLineClear lc (TetrisLevel l) =
  case lc of
    Single -> 100 * l'
    Double -> 300 * l'
    Triple -> 500 * l'
    Tetris -> 800 * l'
    TSpinSingle -> 800 * l'
    TSpinDouble -> 1200 * l'
    TSpinTriple -> 1600 * l'
    MiniTSpinSingle -> 200 * l'
    B2BTSpinSingle -> 1200 * l'
    B2BTSpinDouble -> 1800 * l'
    B2BTSpinTriple -> 2400 * l'
    B2BTetris -> 1200 * l'
  where
    l' = l + 1

textFromLineClear :: LineClear -> [(T.Text, Int)]
textFromLineClear lc =
  case lc of
    Single -> [("Single", 0)]
    Double -> [("Double", 0)]
    Triple -> [("Triple!", 0)]
    Tetris -> [("TETRIS!", 0)]
    TSpinSingle -> [("T-Spin", -24), ("Single", 0)]
    TSpinDouble -> [("T-Spin", -24), ("Double!", 0)]
    TSpinTriple -> [("T-SPIN", -24), ("TRIPLE!!", 0)]
    MiniTSpinSingle -> [("Mini T-Spin", -24), ("Single", 0)]
    B2BTSpinSingle -> [("B2B T-Spin", -24), ("Single!", 0)]
    B2BTSpinDouble -> [("B2B T-Spin", -24), ("DOUBLE!!", 0)]
    B2BTSpinTriple -> [("B2B T-SPIN", -24), ("TRIPLE!!!", 0)]
    B2BTetris -> [("B2B TETRIS!!", 0)]

floatingTextLineClear :: LineClear -> Double -> Double -> (Double, Double) -> [FloatingText]
floatingTextLineClear lc ttl speed (x, y) =
  map (\(t, offsetY) -> makeFloatingText ttl speed (x, y + fromIntegral offsetY) t) texts
  where
    texts = textFromLineClear lc

-- Locks a piece in place. Returns MainStatePhase because this can make us lose the game
-- if a piece gets locked entirely outside of the board.
lockPiece :: PlacingState -> Assets -> GameState -> (MainStatePhase, [SideEffect])
lockPiece ps assets gs
  -- This piece locked outside the board, so gg.
  | defeated =
    let (initGameOver, sideEffs) = initialGameOverState (assets ^. soundAssets) gs
     in (GameOver initGameOver, sideEffs)
  -- Locking this piece made us clear lines.
  | not (null linesToClear) =
    let actualNewBoard = newBoard // map (\r -> (r, array (0, fieldWidth - 1) [(c, Nothing) | c <- [0 .. fieldWidth - 1]])) linesToClear
        newPhase = ClearingLines 0
        newLinesCleared = gs ^. linesCleared + length linesToClear
        levelUpAudio = if levelFromLinesCleared newLinesCleared > levelFromLinesCleared (gs ^. linesCleared) then [PlayAudio (assets ^. soundAssets . levelUpSfx)] else []
        lineClearType = calculateClearType (length linesToClear) (gs ^. maybeLastLockClear) (ps ^. tetromino) (gs ^. board)
        clearScore = scoreFromLineClear lineClearType (levelFromLinesCleared (gs ^. linesCleared))
        statUpdate mv =
          case mv of
            Nothing -> Just 1
            Just x -> Just $ x + 1
        newStats =
          gs ^. gameStats
            & clears %~ Map.alter statUpdate lineClearType
            & maxCombo %~ max newComboCount
        newComboCount = gs ^. comboCount + 1
        comboScore =
          if newComboCount > 1
            then newComboCount * 50 * (l + 1)
            else 0
        newScore = (gs ^. score) + clearScore + comboScore
        newFloatingTexts =
          (gs ^. floatingTexts)
            <> floatingTextLineClear lineClearType 1.5 (-80) (fromIntegral gpX + scoreOffsetX, fromIntegral $ gpY + 600)
            <> [makeFloatingText 1.5 (-80) (fromIntegral gpX + scoreOffsetX, fromIntegral $ gpY + 648) (T.pack $ show clearScore)]
            <> if newComboCount > 1
              then
                [ makeFloatingText 1.5 (-80) (fromIntegral gpX + scoreOffsetX, fromIntegral $ gpY + 500) (T.pack $ show newComboCount <> " Combo!"),
                  makeFloatingText 1.5 (-80) (fromIntegral gpX + scoreOffsetX, fromIntegral $ gpY + 548) (T.pack $ show comboScore)
                ]
              else []
     in ( Game $
            gs
              { _board = actualNewBoard,
                _phase = newPhase,
                _linesCleared = newLinesCleared,
                _maybeLastLockClear = Just lineClearType,
                _score = newScore,
                _floatingTexts = newFloatingTexts,
                _comboCount = newComboCount,
                _gameStats = newStats
              },
          levelUpAudio <> [PlayAudio (assets ^. soundAssets . lineClearSfx)]
        )
  -- Plain old normal locking, send the next piece.
  | otherwise =
    let (newPhase, sideEffects) = sendNextTetromino False assets (gs {_board = newBoard})
        lockType = calculateLockType (ps ^. tetromino) (gs ^. board)
        maybeSpinReward =
          case lockType of
            NormalLock -> Nothing
            MiniTSpinLock -> Just ("Mini T-Spin", 100 * (l + 1))
            TSpinLock -> Just ("T-Spin", 400 * (l + 1))
        spinText =
          case maybeSpinReward of
            Nothing -> []
            Just (t, s) ->
              [ makeFloatingText 1.5 (-80) (fromIntegral gpX + scoreOffsetX, fromIntegral $ gpY + 600) t,
                makeFloatingText 1.5 (-80) (fromIntegral gpX + scoreOffsetX, fromIntegral $ gpY + 648) (T.pack $ show s)
              ]
        addScore gs_ = gs_ & comboCount .~ 0 & score +~ maybe 0 snd maybeSpinReward & floatingTexts <>~ spinText
     in ( case newPhase of
            Game gs' -> Game $ addScore gs'
            GameOver gameOverState -> GameOver $ gameOverState & finishedGame %~ addScore
            _ -> newPhase,
          sideEffects <> [PlayAudio (assets ^. soundAssets . lockedPieceSfx)]
        )
  where
    (bl1, bl2, bl3, bl4) = relativeBlocksFromRotation (ps ^. tetromino . shape) (ps ^. tetromino . rotation)
    (tetI, tetJ) = ps ^. tetromino . pos
    blockPos = map (\(blI, blJ) -> (blI + floor tetI, blJ + tetJ)) [bl1, bl2, bl3, bl4]
    positionsToUpdate = tupleHistogram blockPos
    newBoard = (gs ^. board) // map (\(r, cols) -> (r, ((gs ^. board) ! r) // map (\col -> (col, Just $ ps ^. tetromino . shape)) cols)) positionsToUpdate
    linesToClear = map fst $ filter (\(r, _) -> and (isJust <$> (newBoard ! r))) positionsToUpdate
    (TetrisLevel l) = levelFromLinesCleared (gs ^. linesCleared)
    (gpX, gpY) = gamePosition
    scoreOffsetX = -30
    defeated = all (\(i, _) -> i >= visibleFieldHeight) blockPos

tetrominoVel :: Bool -> TetrisLevel -> Double
tetrominoVel wantsToSoftDrop tl =
  if wantsToSoftDrop then softDropSpeed else normalDropSpeed tl

tickPlacingState :: PlacingState -> Double -> Assets -> GameState -> (MainStatePhase, [SideEffect])
tickPlacingState ps dt assets gs =
  -- First thing is check if we're locking a piece. 'Nothing' means we're not.
  case ps ^. lockingTime of
    Nothing ->
      let currentRow = ps ^. tetromino . pos . _1
          vel = tetrominoVel (gs ^. wantsToSoftDrop) (levelFromLinesCleared (gs ^. linesCleared))
          nextRowPos = currentRow - vel * dt
       in -- This tick did not make us move, just update our position.
          if floor nextRowPos == floor currentRow
            then (Game $ gs & phase .~ Placing (ps & (tetromino . pos . _1) .~ nextRowPos), [])
            else -- Or it did make us move, in which case we check a bunch of things.

              let deltaMove = floor currentRow - floor nextRowPos
                  moves = replicate deltaMove (shiftTetrominoDown (gs ^. board))
                  (movedTetromino, successful) = foldl' (\(t, s) sTD -> let (t', s') = sTD t in (t', s && s')) (ps ^. tetromino, True) moves
                  (_, nextWasAlsoSuccessful) = shiftTetrominoDown (gs ^. board) movedTetromino
                  newLocking = not (successful && nextWasAlsoSuccessful)
                  scoreToAward =
                    if gs ^. wantsToSoftDrop
                      then min (20 - (ps ^. softDropScore)) (floor (ps ^. tetromino . pos . _1) - floor (movedTetromino ^. pos . _1))
                      else 0
                  newTetromino =
                    if successful
                      then (ps ^. tetromino) & (pos . _1) .~ nextRowPos
                      else movedTetromino
                  sideEffects =
                    if not nextWasAlsoSuccessful
                      then [PlayAudio (assets ^. soundAssets . landOnSurfaceSfx)]
                      else []
                  newPs =
                    ps
                      { _tetromino = newTetromino,
                        _lockingTime = if newLocking then Just 0 else Nothing,
                        _softDropScore = _softDropScore ps + scoreToAward
                      }
               in (Game $ gs & phase .~ Placing newPs & score +~ scoreToAward, sideEffects)
    -- We are locking a piece. Count the time to lock and if it expires, lock it in place
    -- and send a new piece to be placed.
    Just t ->
      if t >= timeToLock
        then lockPiece ps assets gs
        else (Game $ gs & phase .~ Placing (ps & lockingTime ?~ (t + dt)), [])

-- Coordinates are in (line, column) form relative to its axis.
relativeBlocksFromRotation :: TetrominoShape -> TetrominoRotation -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
relativeBlocksFromRotation shape rotation =
  case shape of
    J -> case rotation of
      Base -> ((1, -1), (0, -1), (0, 0), (0, 1))
      Ninety -> ((1, 0), (0, 0), (-1, 0), (1, 1))
      OneEighty -> ((-1, 1), (0, -1), (0, 0), (0, 1))
      TwoSeventy -> ((1, 0), (0, 0), (-1, 0), (-1, -1))
    L -> case rotation of
      Base -> ((0, -1), (0, 0), (0, 1), (1, 1))
      Ninety -> ((1, 0), (0, 0), (-1, 0), (-1, 1))
      OneEighty -> ((0, -1), (0, 0), (0, 1), (-1, -1))
      TwoSeventy -> ((1, 0), (0, 0), (-1, 0), (1, -1))
    O -> ((0, 0), (0, 1), (1, 0), (1, 1))
    I -> case rotation of
      Base -> ((0, -1), (0, 0), (0, 1), (0, 2))
      Ninety -> ((1, 1), (0, 1), (-1, 1), (-2, 1))
      OneEighty -> ((-1, -1), (-1, 0), (-1, 1), (-1, 2))
      TwoSeventy -> ((1, 0), (0, 0), (-1, 0), (-2, 0))
    Z -> case rotation of
      Base -> ((1, -1), (1, 0), (0, 0), (0, 1))
      Ninety -> ((1, 1), (0, 0), (0, 1), (-1, 0))
      OneEighty -> ((0, -1), (0, 0), (-1, 0), (-1, 1))
      TwoSeventy -> ((1, 0), (0, -1), (0, 0), (-1, -1))
    S -> case rotation of
      Base -> ((0, -1), (0, 0), (1, 0), (1, 1))
      Ninety -> ((0, 0), (1, 0), (0, 1), (-1, 1))
      OneEighty -> ((-1, -1), (-1, 0), (0, 0), (0, 1))
      TwoSeventy -> ((0, -1), (1, -1), (0, 0), (-1, 0))
    T -> case rotation of
      Base -> ((0, -1), (0, 0), (1, 0), (0, 1))
      Ninety -> ((-1, 0), (0, 0), (1, 0), (0, 1))
      OneEighty -> ((-1, 0), (0, 0), (0, -1), (0, 1))
      TwoSeventy -> ((-1, 0), (0, 0), (0, -1), (1, 0))

tetrominoClockwiseRotation :: TetrominoRotation -> TetrominoRotation
tetrominoClockwiseRotation rot =
  case rot of
    Base -> Ninety
    Ninety -> OneEighty
    OneEighty -> TwoSeventy
    TwoSeventy -> Base

tetrominoCounterclockwiseRotation :: TetrominoRotation -> TetrominoRotation
tetrominoCounterclockwiseRotation rot =
  case rot of
    Base -> TwoSeventy
    Ninety -> Base
    OneEighty -> Ninety
    TwoSeventy -> OneEighty

renderGame :: SDL.Renderer -> Assets -> GameState -> (Int, Int) -> IO ()
renderGame renderer assets gs cameraPos@(cx, cy) =
  do
    renderBoard (assets ^. imageAssets) boardPosition (gs ^. board) renderer
    renderNextShapes (assets ^. imageAssets) (gpX + boardWidthOffset + nextShapesWidthOffset, gpY + nextShapesHeightOffset) (gs ^. nextShapes) renderer
    renderTextureCentered renderer (assets ^. textAssets . levelText) (gpX + textWidthOffset, gpY + levelTextHeightOffset)
    renderTextCentered (assets ^. textAssets . font) renderer (T.pack $ show $ l + 1) (gpX + textWidthOffset, gpY + levelTextHeightOffset + 48)
    renderTextureCentered renderer (assets ^. textAssets . scoreText) (gpX + textWidthOffset, gpY + scoreTextHeightOffset)
    renderTextCentered (assets ^. textAssets . font) renderer (T.pack $ show (gs ^. score)) (gpX + textWidthOffset, gpY + scoreTextHeightOffset + 48)
    case gs ^. heldPiece of
      Just t -> renderTetrominoShape (assets ^. imageAssets) (gpX, gpY + heldPieceHeightOffset) t renderer
      Nothing -> return ()
    case gs ^. phase of
      Placing placingState ->
        do
          renderGhostTetromino (assets ^. imageAssets) boardPosition (placingState ^. tetromino) (gs ^. board) renderer
          renderTetromino boardPosition (placingState ^. tetromino) (colorFromShape (assets ^. imageAssets) (placingState ^. tetromino . shape)) renderer
      _ -> return ()
    mapM_ (renderFloatingText (assets ^. textAssets . font) renderer cameraPos) (gs ^. floatingTexts)
  where
    boardWidthOffset = 128
    nextShapesWidthOffset = fieldWidth * cellSize + 48
    nextShapesHeightOffset = 64
    (gpX, gpY) = (gamePosition ^. _1 - cx, gamePosition ^. _2 - cy)
    boardPosition = (gpX + boardWidthOffset, gpY)
    heldPieceHeightOffset = 64
    textWidthOffset = boardWidthOffset + nextShapesWidthOffset + 192
    levelTextHeightOffset = 64
    scoreTextHeightOffset = 200
    (TetrisLevel l) = levelFromLinesCleared $ gs ^. linesCleared

colorFromShape :: ImageAssets -> TetrominoShape -> SDL.Texture
colorFromShape assets shape =
  case shape of
    I -> assets ^. cyanBlock
    O -> assets ^. yellowBlock
    J -> assets ^. blueBlock
    L -> assets ^. orangeBlock
    S -> assets ^. greenBlock
    Z -> assets ^. redBlock
    T -> assets ^. purpleBlock

renderBoard :: ImageAssets -> (Int, Int) -> Board -> SDL.Renderer -> IO ()
renderBoard assets (x, y) b r =
  do
    renderBorders
    forM_ toRender $ \row ->
      forM_ row $ \((i, j), cell) ->
        renderTexture
          r
          (colorFromShape assets cell)
          (x + cellSize * j, y + boardHeight - cellSize * (i + 1))
  where
    ixArray = zipWith (\i row -> zipWith (\j cell -> ((i, j), cell)) [0 ..] (elems row)) [0 ..] (elems b)
    toRender = filter (not . null) $ take visibleFieldHeight $ map (\r -> [(p, c) | (p, Just c) <- r]) ixArray
    boardHeight = cellSize * visibleFieldHeight
    boardWidth = cellSize * fieldWidth
    renderBorders =
      do
        horTexInfo <- SDL.queryTexture (assets ^. horizontalBorder)
        verTextInfo <- SDL.queryTexture (assets ^. verticalBorder)
        let (borderMinX, borderMaxX) = (x - fromIntegral (SDL.textureWidth horTexInfo), x + boardWidth)
        let (borderMinY, borderMaxY) = (y - fromIntegral (SDL.textureHeight verTextInfo), y + boardHeight)
        renderTexture r (assets ^. horizontalBorder) (borderMinX, borderMinY)
        renderTexture r (assets ^. horizontalBorder) (borderMaxX, borderMinY)
        renderTexture r (assets ^. verticalBorder) (borderMinX, borderMinY)
        renderTexture r (assets ^. verticalBorder) (borderMinX, borderMaxY)

renderNextShapes ::
  ImageAssets ->
  (Int, Int) ->
  (TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape) ->
  SDL.Renderer ->
  IO ()
renderNextShapes assets (x, y) (ns0, ns1, ns2, ns3, ns4, ns5) r =
  forM_ shapes $ \(i, s) ->
    renderTetrominoShape assets (x, y + i * heightSeparation) s r
  where
    shapes = zip [0 ..] [ns0, ns1, ns2, ns3, ns4, ns5]
    heightSeparation = 96

renderTetrominoShape :: ImageAssets -> (Int, Int) -> TetrominoShape -> SDL.Renderer -> IO ()
renderTetrominoShape assets (x, y) shape r =
  forM_ blocks $ \(i, j) ->
    renderTexture
      r
      color
      (fromIntegral (x + cellSize * j), fromIntegral (y - cellSize * (i + 1)))
  where
    color = colorFromShape assets shape
    (bl1, bl2, bl3, bl4) = relativeBlocksFromRotation shape Base
    blocks = [bl1, bl2, bl3, bl4]

renderGhostTetromino :: ImageAssets -> (Int, Int) -> Tetromino -> Board -> SDL.Renderer -> IO ()
renderGhostTetromino assets (x, y) tetromino board =
  renderTetromino (x, y) (dropTetromino board tetromino) (assets ^. grayBlock)

renderTetromino :: (Int, Int) -> Tetromino -> SDL.Texture -> SDL.Renderer -> IO ()
renderTetromino (x, y) tet color r =
  forM_ blocks $ \(i, j) ->
    renderTexture
      r
      color
      (fromIntegral (x + cellSize * (tetJ + j)), fromIntegral (y + boardHeight - cellSize * (itetI + i + 1)))
  where
    boardHeight = cellSize * visibleFieldHeight
    (tetI, tetJ) = tet ^. pos
    itetI = floor tetI
    (bl1, bl2, bl3, bl4) = relativeBlocksFromRotation (tet ^. shape) (tet ^. rotation)
    blocks = filter (\(blI, _) -> (blI + itetI) < visibleFieldHeight) [bl1, bl2, bl3, bl4]