module Draw where

import Anthill
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
circ = circleSolid
rect = rectangleSolid

color_food           = color orange
color_ant f          = color (greyN f)
color_trail IWASHERE = color cyan
color_trail FOOD     = color yellow
color_trail QUEEN    = color violet

draw :: Thing -> Picture
draw (Food amount)    = color_food $ circleSolid amount
draw (Trail t amount) = color_trail t $ circleSolid amount
draw (Ant (Queen {fullness=f})) =
    color_ant f $ pictures [rect 10 10, rotate 45 $ rect 10 10]
draw (Ant (Worker {fullness=f, dir=d})) =
    color_ant f $ pictures [scale 2 1 $ circ 2, translate 5 0 $ circ 2]

draw_world :: World -> Picture
draw_world ts = pictures $ map (\(thing, (x, y)) -> translate x y $ draw thing) ts
