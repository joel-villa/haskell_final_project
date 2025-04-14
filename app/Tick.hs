module Tick where
import Types

tick :: Float -> World -> World
tick _ w = newWorld
  where 
    newPlayer = updatePlayer (hero w)
    newWorld = w {hero = newPlayer}

updatePlayer :: Player -> Player
updatePlayer p0 = newP
  where
    p1 = p0 {xPos = xVel p0 + xPos p0, yPos = yVel p0 + yPos p0}
    newP = if inAir p1 then p1 {yVel = yVel p1 - 10} else p1 -- if in Air, fall