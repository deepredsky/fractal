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

kochSnowFlake n baseLine =
  kochSnowFlake' n side1
  ++ kochSnowFlake' n side2
  ++ kochSnowFlake' n side3
    where
      side1 = Line p1 p2
      side2 = Line p2 p3
      side3 = Line p3 p1
      [p1, p2, p3, _] = shape 3 baseLine

      kochSnowFlake' n baseLine
        | n == 0 = []
        | otherwise = [pS]
                      ++ kochSnowFlake' (n-1) (Line pS p0)
                      ++ kochSnowFlake' (n-1) (Line p0 p2)
                      ++ kochSnowFlake' (n-1) (Line p2 p1)
                      ++ kochSnowFlake' (n-1) (Line p1 pE)
                      ++ [pE]
          where
            Line pS pE = baseLine
            l1 = scaleLine (1/3) baseLine
            l2@(Line p0 p1) = joinLine l1 l1
            Line _ p2 = rotateLine (pi * 5/3) l2
