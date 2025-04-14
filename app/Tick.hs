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
    p1 = p0 {xPos = xPos p0 + xVel p0 , yPos =yPos p0 +yVel p0}
    newP = if inAir p1 then (handleVerticalCollision p1 (-237))  else p1 -- if in Air, fall

handleVerticalCollision:: Player -> Float -> Player 
handleVerticalCollision player pos 
  |pos> yPos player = player{yPos=pos, inAir=False,yVel=0}--too far
  |otherwise = player {yVel = yVel player - 1}
