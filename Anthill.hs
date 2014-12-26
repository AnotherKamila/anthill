module Anthill where

import Data.List
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

-- constants
trail_decay     = 0.95
digestion_speed = 0.02
near_radius     = 15
epsilon         = 1e-4
-- end constants

type Amount  = Float -- non-negative, in ant units
data Message = IWASHERE | FOOD | QUEEN
data Ant     = Worker { fullness :: Float, dir :: Float, msg :: Message }
             | Queen  { fullness :: Float }

new_ant = Worker {fullness=1, dir=0, msg=IWASHERE}

data Thing = Food Amount | Trail Message Amount | Ant Ant
type World = [(Thing, Point)]
type Perceptions = [(Thing, Point)]

(x1,y1) +: (x2,y2) = (x1 + x2, y1 + y2)
(x1,y1) -: (x2,y2) = (x1 - x2, y1 - y2)
dist a b = magV (b -: a)
o = (0,0)

data Action = Add (Thing, Point) | Del (Thing, Point)


step :: Thing -> Perceptions -> [Action]

step (Ant (Queen {fullness=f})) ts = feed ++ trail ++ reproduce where
    feed = [] -- TODO
    trail = [Add (Trail QUEEN 1, o)]
    reproduce = if f > 0.1 then [Add (Ant new_ant, (42,47))] else []

step a@(Ant (Worker {fullness=f, dir=d, msg=t})) ts = feed ++ trail ++ move where
    feed = [] -- TODO
    move = [Del (a,o), Add (a, (0,10))]
    trail = [Add (Trail t 1, o)]

step (Trail t a) _ = [ Del (Trail t a, o), Add (Trail t a*trail_decay, o) ]

step _ _ = []


step_world :: World -> World
step_world w = foldl one_thing w w

one_thing :: World -> (Thing, Point) -> World
one_thing w (t,c) = foldl exec_action (step t (around c w)) where

    around :: Point -> World -> Perceptions
    around c w = map (\(t,p) -> (t,p -: c)) $
                filter (\(_,p) -> dist p c < near_radius) w

    exec_action :: World -> Action -> World
    exec_action ts (Add (t,p)) = (t, p +: c):ts
    exec_action ts (Del (t,p)) = deleteBy same ts where
        same (t2,p2) = t == t2 && dist (p +: c) p2 < epsilon

initial_state :: World
initial_state = [ (Ant (Queen {fullness=5}), (0,0)) ]
