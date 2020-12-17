{-# LANGUAGE OverloadedStrings #-}

module Main where

import Constants (windowSize)
import Control.Lens
import CountingDown
import Game
import InitialStates
import Intro
import Paused
import qualified SDL
import qualified SDL.Font as Ttf
import qualified SDL.Mixer as Mixer
import System.Random (StdGen, getStdGen)
import Types
import UtilsSDL (mainLoop)

main :: IO ()
main =
  mainLoop
    initialMainState
    60
    input
    tick
    render
    "Hetris V2"
    windowSize

initialMainState :: (SDL.Window, SDL.Renderer) -> IO MainState
initialMainState (_, r) =
  do
    ass <- loadAssets r
    Mixer.setVolume 10 Mixer.AllChannels
    let initGameState = initialIntroState
    return $
      MainState
        { _gameAssets = ass,
          _mainPhase = Intro initGameState
        }

loadSoundAssets :: IO SoundAssets
loadSoundAssets =
  do
    hitWall <- Mixer.load "assets/sfx/hitwall.wav"
    lineClear <- Mixer.load "assets/sfx/lineclear.wav"
    landOnSurface <- Mixer.load "assets/sfx/landonsurface.wav"
    lineDrop <- Mixer.load "assets/sfx/linedrop.wav"
    lockedPiece <- Mixer.load "assets/sfx/lockedpiece.wav"
    movement <- Mixer.load "assets/sfx/movement.wav"
    rotation <- Mixer.load "assets/sfx/rotation.wav"
    defeat <- Mixer.load "assets/sfx/defeat.wav"
    pause <- Mixer.load "assets/sfx/pause.wav"
    pauseSelect <- Mixer.load "assets/sfx/pauseselect.wav"
    levelUp <- Mixer.load "assets/sfx/levelup.wav"
    countdown <- Mixer.load "assets/sfx/countdown.wav"
    countdownGo <- Mixer.load "assets/sfx/countdowngo.wav"
    return
      SoundAssets
        { _hitWallSfx = hitWall,
          _lineClearSfx = lineClear,
          _landOnSurfaceSfx = landOnSurface,
          _lineDropSfx = lineDrop,
          _lockedPieceSfx = lockedPiece,
          _movementSfx = movement,
          _rotationSfx = rotation,
          _defeatSfx = defeat,
          _pauseSfx = pause,
          _pauseSelectSfx = pauseSelect,
          _levelUpSfx = levelUp,
          _countdownSfx = countdown,
          _countdownGoSfx = countdownGo
        }

loadImageAssets :: SDL.Renderer -> IO ImageAssets
loadImageAssets renderer =
  do
    cyanBMP <- loadTexture "assets/img/cyanblock.bmp"
    greenBMP <- loadTexture "assets/img/greenblock.bmp"
    blueBMP <- loadTexture "assets/img/blueblock.bmp"
    orangeBMP <- loadTexture "assets/img/orangeblock.bmp"
    redBMP <- loadTexture "assets/img/redblock.bmp"
    purpleBMP <- loadTexture "assets/img/purpleblock.bmp"
    yellowBMP <- loadTexture "assets/img/yellowblock.bmp"
    grayBMP <- loadTexture "assets/img/grayblock.bmp"
    verticalBMP <- loadTexture "assets/img/verticalBorder.bmp"
    horizontalBMP <- loadTexture "assets/img/horizontalBorder.bmp"
    return
      ImageAssets
        { _cyanBlock = cyanBMP,
          _greenBlock = greenBMP,
          _blueBlock = blueBMP,
          _orangeBlock = orangeBMP,
          _redBlock = redBMP,
          _purpleBlock = purpleBMP,
          _yellowBlock = yellowBMP,
          _grayBlock = grayBMP,
          _verticalBorder = verticalBMP,
          _horizontalBorder = horizontalBMP
        }
  where
    loadTexture fp =
      do
        bmpSurf <- SDL.loadBMP fp
        t <- SDL.createTextureFromSurface renderer bmpSurf
        SDL.freeSurface bmpSurf
        return t

loadTextAssets :: SDL.Renderer -> IO TextAssets
loadTextAssets renderer =
  do
    font <- Ttf.load "assets/fonts/PokemonGB.ttf" 20
    levelText <- loadText font "Level"
    scoreText <- loadText font "Score"
    return $
      TextAssets
        { _font = font,
          _levelText = levelText,
          _scoreText = scoreText
        }
  where
    loadText f t =
      do
        surf <- Ttf.blended f (SDL.V4 255 255 255 255) t
        t <- SDL.createTextureFromSurface renderer surf
        SDL.freeSurface surf
        return t

loadAssets :: SDL.Renderer -> IO Assets
loadAssets r =
  Assets
    <$> loadImageAssets r
    <*> loadSoundAssets
    <*> loadTextAssets r

input :: SDL.EventPayload -> MainState -> IO MainState
input ev ms =
  case ms ^. mainPhase of
    Intro is ->
      do
        newPhase <- inputIntro ev (ms ^. gameAssets) is
        return $ ms & mainPhase .~ newPhase
    Paused ps ->
      do
        newPhase <- inputPaused ev (ms ^. gameAssets) ps
        return $ ms & mainPhase .~ newPhase
    CountingDown cds ->
      do
        newPhase <- inputCountingDown ev (ms ^. gameAssets) cds
        return $ ms & mainPhase .~ newPhase
    Game gs ->
      do
        newPhase <- inputGame ev (ms ^. gameAssets) gs
        return $ ms & mainPhase .~ newPhase

tick :: Double -> MainState -> IO MainState
tick dt ms =
  case ms ^. mainPhase of
    Intro is ->
      do
        newPhase <- tickIntro dt (ms ^. gameAssets) is
        return $ ms & mainPhase .~ newPhase
    Paused ps ->
      do
        newPhase <- tickPaused dt ps
        return $ ms & mainPhase .~ newPhase
    CountingDown cds ->
      do
        newPhase <- tickCountingDown dt (ms ^. gameAssets) cds
        return $ ms & mainPhase .~ newPhase
    Game gs ->
      do
        newPhase <- tickGame dt (ms ^. gameAssets) gs
        return $ ms & mainPhase .~ newPhase

render :: SDL.Renderer -> MainState -> IO ()
render renderer ms =
  case ms ^. mainPhase of
    Intro is -> renderIntro renderer (ms ^. gameAssets) is
    Paused ps -> renderPaused renderer (ms ^. gameAssets) ps
    CountingDown cds -> renderCountingDown renderer (ms ^. gameAssets) cds
    Game gs -> renderGame renderer (ms ^. gameAssets) gs
