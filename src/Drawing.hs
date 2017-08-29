module Drawing where

import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations

import Codec.Picture( PixelRGBA8( .. ), writePng )

white      = PixelRGBA8 255  255 255 255
black      = PixelRGBA8   0    0   0 255
blue       = PixelRGBA8   0  110 255 255
red        = PixelRGBA8 255    0   0 255
green      = PixelRGBA8 10  255  10  235
orange     = PixelRGBA8 255  153  0  255
lightgreen = PixelRGBA8  27  230  34 255

drawPicture strokeWidth picture =
  renderDrawing 800 800 black $ do
    mapM_ (\(color, path) -> withTexture (uniformTexture color) (drawPath path)) picture
  where
    drawPath path =
      stroke strokeWidth JoinRound (CapRound, CapRound) $
        polyline path

fadeColor (PixelRGBA8 a b c k)
  | k <= 0 = PixelRGBA8 a b c 0
  | otherwise = PixelRGBA8 a b c (k-1)

scaleLine factor (Line (V2 x1 y1) (V2 x2 y2)) = Line (V2 x1 y1) (V2 x2' y2')
  where
    x0 = x2 - x1
    y0 = y2 - y1
    x2' = x' + x1
    y2' = y' + y1
    x' = factor * x0
    y' = factor * y0

rotateLine angle line@(Line p1 p2) =
  transform (applyTransformation $ rotateCenter angle p1) line

moveLineToPoint startPoint@(V2 x0 y0) l = Line startPoint (V2 x2 y2)
  where
    x2 = x0 + xE - xS
    y2 = y0 + yE - yS
    Line (V2 xS yS) (V2 xE yE) = l

joinLine l1 l2 = moveLineToPoint endPointLine1 l2
  where
    Line _ endPointLine1 = l1

spiral angle scaleFactor n line
  = spiral' n line
    where
      spiral' n l@(Line p1 p2)
        | n <= 0 = []
        | otherwise = p1 : spiral' (n-1) newLine
        where
          newLine = joinLine l modifiedLine
          modifiedLine = (rotateLine angle . scaleLine scaleFactor) l

shape n l@(Line p1 p2)
  | n <= 2 = [p1, p2]
  | n > 2 = spiral angle 1 (n + 1) l
  where
    angle = ( 2 * pi ) / (fromIntegral n)

flipLine (Line pS pE) = Line pE pS
