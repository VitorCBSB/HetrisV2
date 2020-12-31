{-# LANGUAGE OverloadedStrings #-}

module Main where

import Constants (windowSize)
import Control.Lens
import CountingDown
import Credits
import Game
import GameOver
import Help
import InitialStates
import Intro
import MainMenu
import Paused
import qualified SDL
import qualified SDL.Font as Ttf
import qualified SDL.Mixer as Mixer
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
    Mixer.setMusicVolume 10
    let initGameState = initialIntroState
    return
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
    menuSelect <- Mixer.load "assets/sfx/menuselect.wav"
    levelUp <- Mixer.load "assets/sfx/levelup.wav"
    countdown <- Mixer.load "assets/sfx/countdown.wav"
    countdownGo <- Mixer.load "assets/sfx/countdowngo.wav"
    korobeinikiMusic <- Mixer.load "assets/sfx/korobeiniki.ogg"
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
          _menuSelectSfx = menuSelect,
          _levelUpSfx = levelUp,
          _countdownSfx = countdown,
          _countdownGoSfx = countdownGo,
          _korobeiniki = korobeinikiMusic
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

input :: SDL.EventPayload -> MainState -> IO (Maybe MainState)
input ev ms =
  case ms ^. mainPhase of
    Intro is ->
      do
        newPhase <- inputIntro ev (ms ^. gameAssets) is
        return (Just $ ms & mainPhase .~ newPhase)
    MainMenu mms ->
      do
        maybeNewPhase <- inputMainMenu ev (ms ^. gameAssets) mms
        case maybeNewPhase of
          Nothing -> return Nothing
          Just newPhase ->
            return (Just $ ms & mainPhase .~ newPhase)
    Credits ->
      do
        newPhase <- inputCredits ev
        return (Just $ ms & mainPhase .~ newPhase)
    Help ->
      do
        newPhase <- inputHelp ev
        return (Just $ ms & mainPhase .~ newPhase)
    Paused ps ->
      do
        maybeNewPhase <- inputPaused ev (ms ^. gameAssets) ps
        case maybeNewPhase of
          Nothing -> return Nothing
          Just newPhase ->
            return (Just $ ms & mainPhase .~ newPhase)
    CountingDown cds ->
      do
        newPhase <- inputCountingDown ev (ms ^. gameAssets) cds
        return (Just $ ms & mainPhase .~ newPhase)
    Game gs ->
      do
        newPhase <- inputGame ev (ms ^. gameAssets) gs
        return (Just $ ms & mainPhase .~ newPhase)
    GameOver gos ->
      do
        newPhase <- inputGameOver ev (ms ^. gameAssets) gos
        return (Just $ ms & mainPhase .~ newPhase)

tick :: Double -> MainState -> IO (Maybe MainState)
tick dt ms =
  case ms ^. mainPhase of
    Intro is ->
      do
        newPhase <- tickIntro dt (ms ^. gameAssets) is
        return (Just $ ms & mainPhase .~ newPhase)
    MainMenu mms ->
      do
        newPhase <- tickMainMenu dt mms
        return (Just $ ms & mainPhase .~ newPhase)
    Credits ->
      do
        newPhase <- tickCredits dt
        return (Just $ ms & mainPhase .~ newPhase)
    Help ->
      do
        newPhase <- tickHelp dt
        return (Just $ ms & mainPhase .~ newPhase)
    Paused ps ->
      do
        newPhase <- tickPaused dt ps
        return (Just $ ms & mainPhase .~ newPhase)
    CountingDown cds ->
      do
        newPhase <- tickCountingDown dt (ms ^. gameAssets) cds
        return (Just $ ms & mainPhase .~ newPhase)
    Game gs ->
      do
        newPhase <- tickGame dt (ms ^. gameAssets) gs
        return (Just $ ms & mainPhase .~ newPhase)
    GameOver gos ->
      do
        newPhase <- tickGameOver dt gos
        return (Just $ ms & mainPhase .~ newPhase)

render :: SDL.Renderer -> MainState -> IO ()
render renderer ms =
  case ms ^. mainPhase of
    Intro is -> renderIntro renderer (ms ^. gameAssets) is
    MainMenu mms -> renderMainMenu renderer (ms ^. gameAssets) mms
    Credits -> renderCredits renderer (ms ^. gameAssets)
    Help -> renderHelp renderer (ms ^. gameAssets)
    Paused ps -> renderPaused renderer (ms ^. gameAssets) ps
    CountingDown cds -> renderCountingDown renderer (ms ^. gameAssets) cds
    Game gs -> renderGame renderer (ms ^. gameAssets) gs
    GameOver gos -> renderGameOver renderer (ms ^. gameAssets) gos
