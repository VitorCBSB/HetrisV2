module UtilsSDL
  ( mainLoop
  , renderTexture
  , renderTextureCentered
  , renderText
  , renderTextCentered
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (foldM, unless)
import Data.Word (Word32)
import qualified Data.Text as T
import Linear (V4(V4))
import qualified SDL
import qualified SDL.Mixer as Mixer
import qualified SDL.Font as Ttf

mainLoop 
  :: ((SDL.Window, SDL.Renderer) -> IO world) -- Initial state
  -> Word32 -- FPS
  -> (SDL.EventPayload -> world -> IO world) -- Event response function
  -> (Double -> world -> IO world) -- Tick function
  -> (SDL.Renderer -> world -> IO ()) -- Render function
  -> T.Text -- Window name
  -> (Int, Int) -- Window size
  -> IO ()
mainLoop init fps event tick render windowName windowSize =
  withInitialization windowName windowSize $ \s ->
    initializedLoop init fps event tick render s

initializedLoop 
  :: ((SDL.Window, SDL.Renderer) -> IO world) -- Initial state
  -> Word32 -- FPS
  -> (SDL.EventPayload -> world -> IO world) -- Event response function
  -> (Double -> world -> IO world) -- Tick function
  -> (SDL.Renderer -> world -> IO ()) -- Render function
  -> (SDL.Window, SDL.Renderer)
  -> IO ()
initializedLoop init fps event tick render ctx@(_, r) =
  init ctx >>= loop
  where
    frameTime = 1 / fromIntegral fps
    loop s = 
      do  start <- SDL.ticks
          events <- fmap SDL.eventPayload <$> SDL.pollEvents
          postEv <- foldM (flip event) s events
          postTick <- tick frameTime postEv
          (SDL.clear r >> render r postTick >> SDL.present r)
          end <- SDL.ticks
          regulateFPS fps start end
          unless (SDL.QuitEvent `elem` events) (loop postTick)

-- | Will wait until ticks pass
regulateFPS :: Word32 -> Word32 -> Word32 -> IO ()
regulateFPS fps startMs endMs =
    threadDelay $ timeToSleepus fps startMs endMs

timeToSleepus :: Word32 -> Word32 -> Word32 -> Int
timeToSleepus fps startMs endMs
  | fps == 0 = 0
  | otherwise = 
    let
      msPerFrame = 1000.0 / (fromIntegral fps)
      intervalMs = endMs - startMs
      gap = msPerFrame - (fromIntegral intervalMs)
      delayFor = max 0 gap
    in
      floor $ delayFor * 1000

withInitialization :: T.Text -> (Int, Int) -> ((SDL.Window, SDL.Renderer) -> IO ()) -> IO ()
withInitialization windowName (winWidth, winHeight) =
  bracket init end
  where
    init = 
      do  SDL.initialize [SDL.InitVideo]
          Mixer.openAudio Mixer.defaultAudio 1024
          Ttf.initialize
          w <- SDL.createWindow windowName (SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 (fromIntegral winWidth) (fromIntegral winHeight)})
          SDL.showWindow w
          r <- SDL.createRenderer w (-1) SDL.defaultRenderer
          SDL.rendererDrawColor r SDL.$= SDL.V4 0 0 0 0 -- black background
          pure (w, r)
    end (w, r) = 
      do  SDL.destroyRenderer r
          SDL.destroyWindow w
          Ttf.quit
          Mixer.closeAudio
          SDL.quit

renderTexture :: SDL.Renderer -> SDL.Texture -> (Int, Int) -> IO ()
renderTexture renderer texture (x, y) =
    do  texInfo <- SDL.queryTexture texture
        SDL.copy 
            renderer
            texture
            Nothing
            (Just $ SDL.Rectangle (SDL.P $ SDL.V2 (fromIntegral x) (fromIntegral y)) (fromIntegral <$> SDL.V2 (SDL.textureWidth texInfo) (SDL.textureHeight texInfo)))

renderTextureCentered :: SDL.Renderer -> SDL.Texture -> (Int, Int) -> IO ()
renderTextureCentered renderer texture (x, y) =
    do  texInfo <- SDL.queryTexture texture
        SDL.copy 
            renderer
            texture
            Nothing
            (Just $ SDL.Rectangle 
                      (SDL.P $ SDL.V2 (fromIntegral x - (SDL.textureWidth texInfo `div` 2)) (fromIntegral y - SDL.textureHeight texInfo `div` 2)) 
                      (fromIntegral <$> SDL.V2 (SDL.textureWidth texInfo) (SDL.textureHeight texInfo)))

renderText :: Ttf.Font -> SDL.Renderer -> T.Text -> (Int, Int) -> IO ()
renderText f r t p =
    do  surf <- Ttf.solid f (V4 255 255 255 255) t
        texture <- SDL.createTextureFromSurface r surf
        SDL.freeSurface surf
        renderTexture r texture p
        SDL.destroyTexture texture

renderTextCentered :: Ttf.Font -> SDL.Renderer -> T.Text -> (Int, Int) -> IO ()
renderTextCentered f r t p =
    do  surf <- Ttf.solid f (V4 255 255 255 255) t
        texture <- SDL.createTextureFromSurface r surf
        SDL.freeSurface surf
        renderTextureCentered r texture p
        SDL.destroyTexture texture
