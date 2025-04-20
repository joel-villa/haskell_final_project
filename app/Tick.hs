module Tick where
import Types

tick :: Float -> World -> World
tick _ w = newWorld
  where 
    bs = terrain (curLevel w)
    newPlayer = updatePlayer (hero w) (offset w) bs
    newWorld = w {hero = newPlayer, offset=(getOffset w),intro= ((intro w) +1),enemies= (updateEnemies (enemies w))} 

updateEnemies:: [BadGuy]->[BadGuy]
updateEnemies [] =[]
updateEnemies (bg:bgs) = newBg : updateEnemies bgs
  where
    newBg = bg{pathing=(updatePath (pathing bg)) }
    

updatePath :: JPath -> JPath  -- Just switch the starter and end and flip the velocity 
updatePath path =if (xp,yp) == (goalPos path) then newPath else path{x=(x path)+(xVelocity path),y=(y path)+(yVelocity path)}
  where 
    (xp,yp)=((x path), (y path))
    prevGoal = goalPos path
    newPath= path{goalPos=(initPos path), initPos =prevGoal, xVelocity= (xVelocity path) *(-1), yVelocity= (yVelocity path) *(-1)}

updatePlayer :: Player -> Float->[JBlock] -> Player
updatePlayer p0 offs bs = newP
  where
    p1 = p0 {xPos = xPos p0 + xVel p0 , yPos =yPos p0 +yVel p0}
    p2 = if inAir p1 then p1 {yVel = yVel p1 - 0.5} else p1 -- if in Air, fall
    newP = horizontalCollision p2 offs bs

getOffset :: World -> Float 
getOffset w= 
  if ((xPos (hero w))-(offset w)>= (200) && (xVel (hero w))>0) ||((xPos (hero w))-(offset w)<=(200)&&xVel (hero w)<0) then (offset w) +(xVel (hero w)) else (offset w)


--(Line [((2*x1-50), ((2)*y1)), ((2*x2-22), (2*y2))]) 
horizontalCollision :: Player -> Float->[JBlock] -> Player
horizontalCollision p _ [] = p
horizontalCollision p offs (block:bs) = 
  -- These numbers are NOT choosen arbituarly
  -- the 2* is for the scalar of the block
  -- (-50) is how many pixels off the left side of the block picture doesnt take up
  -- (-22) is how many pixels the right side of the block picture doesnt take up
  --If you change this, it will break collison in its current form
  if inBetween (xPos p)  (2*x1-50-offs) (2*x2-22-offs) && inBetween (yPos p) ((2)*y1) (2*y2) --TODO make this not hard-coded :(  x1-25.5), y1), ((x2-18.5)
  then p {yPos = ((2)*y1+85), inAir = False, yVel = 0}
  else horizontalCollision p offs bs
    where 
      (x1, y2) = topLeft block
      x2 = x1 + width block
      y1 = y2 - height block


 
inBetween :: Float -> Float -> Float -> Bool
inBetween x low high = x > low && x < high
--TODO collisions!!


