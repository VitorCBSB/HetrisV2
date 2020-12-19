{-# LANGUAGE TemplateHaskell #-}

module Types
-- ImageAssets and lenses
  ( ImageAssets (..),
    cyanBlock,
    blueBlock,
    redBlock,
    orangeBlock,
    purpleBlock,
    greenBlock,
    yellowBlock,
    grayBlock,
    horizontalBorder,
    verticalBorder,
    -- Sound assets and lenses
    SoundAssets (..),
    hitWallSfx,
    landOnSurfaceSfx,
    lineClearSfx,
    lineDropSfx,
    lockedPieceSfx,
    movementSfx,
    rotationSfx,
    defeatSfx,
    pauseSfx,
    menuSelectSfx,
    levelUpSfx,
    countdownSfx,
    countdownGoSfx,
    -- Text assets and lenses
    TextAssets (..),
    font,
    levelText,
    scoreText,
    -- All assets
    Assets (..),
    imageAssets,
    soundAssets,
    textAssets,
    Board (..),
    -- Main game state
    MainState (..),
    MainStatePhase (..),
    mainPhase,
    gameAssets,
    -- Main menu state
    MainMenuOption (..),
    MainMenuState (..),
    mainMenuSelectedOption,
    -- Tetris game state and lenses
    GameState (..),
    board,
    nextShapes,
    nextTetrominoBag,
    heldPiece,
    phase,
    linesCleared,
    maybeLastLockClear,
    score,
    comboCount,
    wantsToSoftDrop,
    rand,
    floatingTexts,
    GamePhase (..),
    TetrisLevel (..),
    LineClear (..),
    LockType (..),
    newTetrominoBag,
    -- Main game sub-state Placing's state and lenses
    PlacingState (..),
    tetromino,
    fromHeld,
    lockingTime,
    softDropScore,
    hardDropScore,
    SideEffect (..),
    interpretEffects,
    applySideEffect,
    TetrominoShape (..),
    TetrominoRotation (..),
    -- Counting down state, either after pausing or right at the start of a game
    CountingDownState (..),
    gameStateToReturnTo,
    countdown,
    countdownTime,
    -- Intro state
    IntroState (..),
    introTime,
    -- Pause state, for when we are... paused.
    PauseState (..),
    PauseOption (..),
    backgroundGS,
    selectedOption,
    -- Tetromino to place and lenses
    Tetromino (..),
    pos,
    shape,
    rotation,
  )
where

import Control.Lens (makeLenses)
import Data.Array (Array)
import FloatingText (FloatingText)
import qualified SDL
import qualified SDL.Font as Ttf
import qualified SDL.Mixer as Mixer
import System.Random (RandomGen, StdGen)
import Utils (fisherYatesShuffle)

data ImageAssets = ImageAssets
  { _cyanBlock :: SDL.Texture,
    _blueBlock :: SDL.Texture,
    _redBlock :: SDL.Texture,
    _orangeBlock :: SDL.Texture,
    _purpleBlock :: SDL.Texture,
    _greenBlock :: SDL.Texture,
    _yellowBlock :: SDL.Texture,
    _grayBlock :: SDL.Texture,
    _horizontalBorder :: SDL.Texture,
    _verticalBorder :: SDL.Texture
  }

data SoundAssets = SoundAssets
  { _hitWallSfx :: Mixer.Chunk,
    _landOnSurfaceSfx :: Mixer.Chunk,
    _lineClearSfx :: Mixer.Chunk,
    _lineDropSfx :: Mixer.Chunk,
    _lockedPieceSfx :: Mixer.Chunk,
    _movementSfx :: Mixer.Chunk,
    _rotationSfx :: Mixer.Chunk,
    _defeatSfx :: Mixer.Chunk,
    _pauseSfx :: Mixer.Chunk,
    _menuSelectSfx :: Mixer.Chunk,
    _levelUpSfx :: Mixer.Chunk,
    _countdownSfx :: Mixer.Chunk,
    _countdownGoSfx :: Mixer.Chunk
  }

data TextAssets = TextAssets
  { _font :: Ttf.Font,
    _levelText :: SDL.Texture,
    _scoreText :: SDL.Texture
  }

data Assets = Assets
  { _imageAssets :: ImageAssets,
    _soundAssets :: SoundAssets,
    _textAssets :: TextAssets
  }

type Board = Array Int (Array Int (Maybe Block))

newtype TetrisLevel = TetrisLevel Int
  deriving (Eq, Ord)

data MainState = MainState
  { _gameAssets :: Assets,
    _mainPhase :: MainStatePhase
  }

data MainStatePhase
  = Intro IntroState
  | MainMenu MainMenuState
  | CountingDown CountingDownState
  | Game GameState
  | Paused PauseState

data MainMenuOption
  = Marathon
  | Help
  | Credits
  | MainQuitGame
  deriving (Eq)

data MainMenuState = MainMenuState
  { _mainMenuSelectedOption :: MainMenuOption
  }

data IntroState = IntroState
  { _introTime :: Double
  }

data CountingDownState = CountingDownState
  { _gameStateToReturnTo :: GameState,
    _countdown :: Int,
    _countdownTime :: Double
  }

data PauseOption
  = ResumeGame
  | RestartGame
  | BackToMainMenu
  | PauseQuitGame
  deriving (Eq)

data PauseState = PauseState
  { _backgroundGS :: GameState,
    _selectedOption :: PauseOption
  }

data GameState = GameState
  { _board :: Board,
    _nextShapes :: (TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape),
    _nextTetrominoBag :: (Maybe TetrominoShape, Maybe TetrominoShape, Maybe TetrominoShape, Maybe TetrominoShape, Maybe TetrominoShape, Maybe TetrominoShape, Maybe TetrominoShape),
    _heldPiece :: Maybe TetrominoShape,
    _phase :: GamePhase,
    _linesCleared :: Int, -- Used to determine level
    _maybeLastLockClear :: Maybe LineClear,
    _wantsToSoftDrop :: Bool,
    _score :: Int,
    _comboCount :: Int,
    _rand :: StdGen,
    _floatingTexts :: [FloatingText]
  }

data LockType
  = NormalLock
  | TSpinLock
  | MiniTSpinLock
  deriving (Eq)

data LineClear
  = Single
  | Double
  | Triple
  | Tetris
  | TSpinSingle
  | TSpinDouble
  | TSpinTriple
  | MiniTSpinSingle
  | B2BTSpinSingle
  | B2BTetris
  | B2BTSpinDouble
  | B2BTSpinTriple
  deriving (Eq)

data GamePhase
  = Placing PlacingState
  | ClearingLines Double -- Time
  | Defeat Double -- Time

data PlacingState = PlacingState
  { _tetromino :: Tetromino,
    _fromHeld :: Bool,
    _lockingTime :: Maybe Double,
    _softDropScore :: Int,
    _hardDropScore :: Int
  }

data SideEffect
  = PlayAudio Mixer.Chunk

data TetrominoShape
  = L
  | O
  | I
  | S
  | Z
  | T
  | J
  deriving (Eq, Enum, Bounded, Show)

-- Clockwise from the first value to last.
data TetrominoRotation
  = Base
  | Ninety
  | OneEighty
  | TwoSeventy
  deriving (Eq, Enum, Bounded)

data Tetromino = Tetromino
  { _pos :: (Double, Int), -- (line, column)
    _shape :: TetrominoShape,
    _rotation :: TetrominoRotation
  }

type Block = TetrominoShape

$(makeLenses ''MainState)
$(makeLenses ''GameState)
$(makeLenses ''CountingDownState)
$(makeLenses ''IntroState)
$(makeLenses ''PauseState)
$(makeLenses ''MainMenuState)
$(makeLenses ''PlacingState)
$(makeLenses ''Tetromino)
$(makeLenses ''ImageAssets)
$(makeLenses ''SoundAssets)
$(makeLenses ''TextAssets)
$(makeLenses ''Assets)

newTetrominoBag ::
  RandomGen g =>
  g ->
  ((TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape, TetrominoShape), g)
newTetrominoBag g =
  ((sh0, sh1, sh2, sh3, sh4, sh5, sh6), g')
  where
    ([sh0, sh1, sh2, sh3, sh4, sh5, sh6], g') = fisherYatesShuffle g [L, I, O, S, Z, T, J]

interpretEffects :: IO (a, [SideEffect]) -> IO a
interpretEffects act =
  do
    (a, effects) <- act
    mapM_ applySideEffect effects
    return a

applySideEffect :: SideEffect -> IO ()
applySideEffect (PlayAudio chunk) = Mixer.play chunk
