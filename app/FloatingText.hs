{-# LANGUAGE TemplateHaskell #-}

module FloatingText
  ( FloatingText
  , makeFloatingText
  , tickFloatingText
  , renderFloatingText
  )
  where

import UtilsSDL (renderTextCentered)

import Control.Lens
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font as Ttf

data FloatingText = FloatingText
  { _timeToLive :: Double
  , _upwardsSpeed :: Double
  , _pos :: (Double, Double)
  , _text :: T.Text
  }

$(makeLenses ''FloatingText)

makeFloatingText :: Double -> Double -> (Double, Double) -> T.Text -> FloatingText
makeFloatingText = FloatingText

tickFloatingText :: Double -> FloatingText -> Maybe FloatingText
tickFloatingText dt ft =
  if ft ^. timeToLive <= 0 then
    Nothing
  else
    Just $
      ft & timeToLive -~ dt
         & pos . _2 +~ (ft ^. upwardsSpeed * dt)

renderFloatingText :: Ttf.Font -> SDL.Renderer -> FloatingText -> IO ()
renderFloatingText f r ft =
  renderTextCentered f r (ft ^. text) ((ft ^. pos) & both %~ floor)