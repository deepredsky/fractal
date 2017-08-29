module Fractal where

import Drawing
import Graphics.Rasterific

fractalTree factor n baseLine = fractalTree' n baseLine
  where
   fractalTree' n baseLine
     | n == 0 = []
     | otherwise = [p1,p4]
                   ++ fractalTree' (n-1) (Line p4 p5)
                   ++ fractalTree' (n-1) (Line p5 p3)
                   ++ [p3, p2]
       where
         [p1,p2,p3,p4,_] = shape 4 baseLine
         r = (flipLine . scaleLine 0.5) (Line p3 p4)
         Line _ p5 = rotateLine (factor'  * pi) r
         factor' = if n `mod` 4 == 0
                      then factor
                      else (1-factor)
