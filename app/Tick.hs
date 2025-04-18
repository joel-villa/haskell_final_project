module Tick where
import Types

tick :: Float -> World -> World
tick _ w = newWorld
  where 
    bs = terrain (curLevel w)
    newPlayer = updatePlayer (hero w) bs
    newWorld = w {hero = newPlayer} 

updatePlayer :: Player -> [JBlock] -> Player
updatePlayer p0 bs = newP
  where
    p1 = p0 {xPos = xPos p0 + xVel p0 , yPos =yPos p0 +yVel p0}
    p2 = if inAir p1 then p1 {yVel = yVel p1 - 0.5} else p1 -- if in Air, fall
    newP = horizontalCollision p2 bs

horizontalCollision :: Player -> [JBlock] -> Player
horizontalCollision p [] = p
horizontalCollision p (block:bs) = 
  if inBetween (xPos p)  x1 x2 && inBetween (yPos p) (y1) (y2) --TODO make this not hard-coded :(
  then p {yPos = y1, inAir = False, yVel = 0}
  else horizontalCollision p bs
    where 
      (x1, y2) = topLeft block
      x2 = x1 + width block
      y1 = y2 - height block


inBetween :: Float -> Float -> Float -> Bool
inBetween x low high = x > low && x < high
--TODO collisions!!
handleVerticalCollision:: Player -> Float -> Player 
handleVerticalCollision player pos 
  |pos> yPos player = player{yPos=pos, inAir=False,yVel=0}--too far
  |otherwise = player {yVel = yVel player - 1}
