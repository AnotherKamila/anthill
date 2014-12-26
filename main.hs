import Anthill (World, step, initial_state)
import Draw (draw_world)
import Graphics.Gloss.Interface.Pure.Simulate

-- kinda settings
speed   = 10 -- steps/second
display = InWindow "anthill" (1200, 800) (50, 50)
-- end kinda settings

main = simulate display black speed initial_state draw_world (\_ _ m -> step m)
