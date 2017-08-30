module Main where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Fractal
import Drawing

main :: IO ()
main = do
    let fractal_tree_1 = drawPicture 1 [(red, fractalTree 0.6 12 (Line (V2 460 750) (V2 350 750)))]
    let fractal_tree_2 = drawPicture 1 [(blue, fractalTree 0.4 12 (Line (V2 460 750) (V2 350 750)))]
    let fractal_tree_3 = drawPicture 1 [(green, fractalTree 0.55 13 (Line (V2 460 750) (V2 350 750)))]
    let fractal_tree_4 = drawPicture 1 [(lightgreen, fractalTree 0.445 13 (Line (V2 460 750) (V2 350 750)))]
    let fractal_tree_5 = drawPicture 1 [(orange, fractalTree 0.59 12 (Line (V2 460 750) (V2 350 750)))]

    writePng "examples/fractal_tree_1.png" fractal_tree_1
    writePng "examples/fractal_tree_2.png" fractal_tree_2
    writePng "examples/fractal_tree_3.png" fractal_tree_3
    writePng "examples/fractal_tree_4.png" fractal_tree_4
    writePng "examples/fractal_tree_5.png" fractal_tree_5

    let koch_snow_flake = drawPicture 1 [(red, kochSnowFlake 6 (Line (V2 600 550) (V2 200 550)))]
    writePng "examples/koch_snow_flake.png" koch_snow_flake

    let sierpinski_sieve = drawPicture 1 $ sierpinskiSieve blue 6 (Line (V2 650 600) (V2 150 600))
    writePng "examples/sierpinski_sieve.png" sierpinski_sieve
