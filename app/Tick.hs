module Tick where
import Types

tick :: Float -> World -> World
tick _ w = newWorld
  where 
    bs = terrain (curLevel w)
    newPlayer = handleFall (updatePlayer (hero w) (offset w) bs)
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
    p1 = p0 {xPos = xPos p0 + xVel p0 , yPos =yPos p0 +yVel p0} -- new location based on velocities
    p2 = horizontalCollision p1 offs bs                         -- call to horizontalCollision
    p3 = if inAir p2 then p2 {yVel = yVel p2 - 0.25} else p2    -- if in Air, fall
    newP = p3 {weapon = updateWeapon p3}                        -- update Player's weapon velocity

updateWeapon :: Player -> Item
updateWeapon p = oldWeapon {wVelocity = newWVelocity}
  where
    oldWeapon = (weapon p)                          -- the old weapon
    oldV = wVelocity oldWeapon                      -- the old weapon velocity
    newWVelocity = if oldV > 0 then oldV - 1 else 0 -- decrement weapon velocity


--New and improved and critic proof getOffset. Now goes off of the xvel instead of just positioning
getOffset :: World -> Float 
getOffset w= 
  if ((xPos (hero w))-(offset w)>= (200) && (xVel (hero w))>0) ||((xPos (hero w))-(offset w)<=(200)&&xVel (hero w)<0) then (offset w) +(xVel (hero w)) else (offset w)

--TODO, should offset be in tick, I feel like it is more well suited for drawing, but idk
horizontalCollision :: Player -> Float->[JBlock] -> Player
horizontalCollision p _ [] = if not (inAir p) then  p {inAir =True} else p
horizontalCollision p offs (block:bs) = 
  -- These numbers are NOT choosen arbituarly
  -- the 2* is for the scalar of the block
  -- (-50) is how many pixels off the left side of the block picture doesnt take up
  -- (-22) is how many pixels the right side of the block picture doesnt take up
  --If you change this, it will break collison in its current form
  -- TODO: should we move this pixel math into draw? 
  if collision &&  ((yVel p) < 0.25) -- 0.25 b/c 0 resulted in jumping bug
    -- falling onto a block
    then p {yPos = ((2)*y1+85), inAir = False, yVel = 0} 
  else if collision && ((yVel p ) > 0.25) -- 0.25 b/c 0 resulted in jumping bug
    -- hitting head on block
    then p {yPos = (y1 - 120), inAir = True, yVel = 0} -- Somewhat arbitrary 120: TODO: fix inBlock to make this more fluid
  --TODO check both sides of the block as well (rename this function to collision, instead of just horizontalCollision)
  else horizontalCollision p offs bs
    -- (x1, y1) is bottom left corner (min values)
    -- (x2, y2) is top right (max values)
    where 
      collision = inBlock block (xPos p, yPos p) offs 
      (x1, y2) = topLeft block
      y1 = y2 - height block
      
inBlock :: JBlock -> (Float, Float) -> Float-> Bool
-- These numbers are NOT choosen arbituarly
-- the 2* is for the scalar of the block
-- (-50) is how many pixels off the left side of the block picture doesnt take up
-- (-22) is how many pixels the right side of the block picture doesnt take up
--If you change this, it will break collison in its current form
-- Joel's Note: 2*y2 + 80, where 80 was found by guess and check
-- TODO: should we move this pixel math into draw? 
inBlock block (x, y) offs = inBetween x  (2*x1-50-offs) (2*x2-22-offs) && inBetween y ((2)*y1) (2*y2 + 80) --TODO make this not hard-coded :(  x1-25.5), y1), ((x2-18.5)
  -- (x1, y1) is bottom left corner (min values)
  -- (x2, y2) is top right (max values)
  where 
    (x1, y2) = topLeft block
    x2 = x1 + width block
    y1 = y2 - height block

--checks if there is enough lives to bring the player back and if it past the screen height
handleFall::Player->Player
handleFall p =
  if health p >0 &&  yPos p <(-500) then p{health= (health p) -1, yPos =500} else p
 
inBetween :: Float -> Float -> Float -> Bool
inBetween x low high = x > low && x < high



