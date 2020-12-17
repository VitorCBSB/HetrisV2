module Constants
  ( fieldHeight,
    visibleFieldHeight,
    fieldWidth,
    cellSize,
    softDropSpeed,
    timeToLock,
    timeToRestart,
    clearingLinesTime,
    linesLevelUp,
    gamePosition,
    windowSize,
  )
where

fieldHeight :: Int
fieldHeight = 30

visibleFieldHeight :: Int
visibleFieldHeight = 20

fieldWidth :: Int
fieldWidth = 10

cellSize :: Int
cellSize = 32

-- In cells per second
softDropSpeed :: Double
softDropSpeed = 40

-- Time (in seconds) needed to wait until a piece is locked in place.
timeToLock :: Double
timeToLock = 0.5

-- Time (in seconds) whereafter you can press 'Enter' to restart the game.
timeToRestart :: Double
timeToRestart = 2.25

-- In seconds.
clearingLinesTime :: Double
clearingLinesTime = 0.5

-- Line clears needed to level up
linesLevelUp :: Int
linesLevelUp = 10

gamePosition :: (Int, Int)
gamePosition = (200, 25)

windowSize :: (Int, Int)
windowSize = (1024, 768)
