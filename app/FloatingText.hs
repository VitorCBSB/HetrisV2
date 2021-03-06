{-# LANGUAGE TemplateHaskell #-}

module FloatingText
  ( FloatingText,
    makeFloatingText,
    tickFloatingText,
    renderFloatingText,
  )
where

import Control.Lens
import qualified Data.Text as T
import qualified SDL
import qualified SDL.Font as Ttf
import UtilsSDL (renderTextCentered)

data FloatingText = FloatingText
  { _timeToLive :: Double,
    _upwardsSpeed :: Double,
    _pos :: (Double, Double),
    _text :: T.Text
  }

$(makeLenses ''FloatingText)

makeFloatingText :: Double -> Double -> (Double, Double) -> T.Text -> FloatingText
makeFloatingText = FloatingText

tickFloatingText :: Double -> FloatingText -> Maybe FloatingText
tickFloatingText dt ft =
  if ft ^. timeToLive <= 0
    then Nothing
    else
      Just $
        ft & timeToLive -~ dt
          & pos . _2 +~ (ft ^. upwardsSpeed * dt)

renderFloatingText :: Ttf.Font -> SDL.Renderer -> (Int, Int) -> FloatingText -> IO ()
renderFloatingText f r (cx, cy) ft =
  renderTextCentered f r (ft ^. text) (px, py)
  where
    (tempX, tempY) = (ft ^. pos) & both %~ floor
    (px, py) = (tempX - cx, tempY - cy)