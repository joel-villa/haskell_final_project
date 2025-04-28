module Tick where
import Types
import Init

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
    p2 = horzCollisionHitBox p1 offs bs                               -- call to horzCollisionHitBox
    p3 =vertCollision p2 offs bs                                      -- call to vertcollision
    p4 = if inAir p3 then p3 {yVel = yVel p3 - 0.75} else p3    -- if in Air, fall
    (x,y)= topLt (hitBox p4)
    tpl= ((xPos p4), (yPos p4))
    newHit= newHitBox (xPos p4) (yPos p4) (facingRight p4)
    newP = p4 {weapon = updateWeapon p4,hitBox=newHit }                        -- update Player's weapon velocity


updateWeapon :: Player -> Item
updateWeapon p = oldWeapon {wVelocity = newWVelocity, active = newActive}
  where
    oldWeapon = (weapon p)                          -- the old weapon
    oldV = wVelocity oldWeapon                      -- the old weapon velocity
    newWVelocity = if oldV > 0 then oldV - 1 else 0 -- decrement weapon velocity
    newActive = if newWVelocity == 0 then False else True -- weapon is or is not being used 


--New and improved and not critic proof getOffset. Now goes off of the xvel instead of just positioning
getOffset :: World -> Float 
getOffset w= 
  if ((xPos (hero w))-(offset w)>= (200) && (xVel (hero w))>0) ||((xPos (hero w))-(offset w)<=(200)&&xVel (hero w)<0) then (offset w) +(xVel (hero w)) else (offset w)

horzCollisionHitBox:: Player -> Float->[JBlock]->Player
horzCollisionHitBox p _ [] = p {inAir =True}
horzCollisionHitBox p offs (block:bs)=
  if onTop block p offs then p{hitBox=newBox,inAir = False, yPos=y+40} 
  else if hittingHead block p offs then p{yPos = y-40, inAir = True, yVel = 0} 
  else horzCollisionHitBox p offs bs
    where
      (x,y)=topLt (floorBox block)
      (px,py)=bottomLt (hitBox p)
      newBox= updateHitboxB px y (facingRight p)

hittingHead :: JBlock-> Player-> Float ->Bool
hittingHead block p offs =(inBetween farLft (low-offs) (high-offs) || (inBetween farRt (low-offs) (high-offs)))  && (inBetween y y1 y2) && ((yVel p ) >1)
  where
    (farLft, _)= bottomLt (hitBox p)
    (farRt, y) = topRt (hitBox p)
    (low, y1)=(bottomLt (floorBox block))
    (high,y2)=(topRt (floorBox block))


onTop :: JBlock -> Player ->Float-> Bool
onTop block p offs = (inBetween farLft (low-offs) (high-offs) || (inBetween farRt (low-offs) (high-offs) ))&& (((yVel p) < 0.25) && (inBetween y y1 y2))
  where
    (farLft, y)= bottomLt (hitBox p)
    (farRt, _) = bottomRt (hitBox p)
    (low, y1)=(bottomLt (floorBox block))
    (high,y2)=(topRt (floorBox block))

--Not at all functional
vertCollision:: Player -> Float -> [JBlock]->Player 
vertCollision p _ [] =p 
vertCollision p offs (block:bs)=
  if collidingL then p{xPos = x1, hitBox=newBox x1} -- x1 
  else if False then p{xPos =x2,hitBox=newBox x2} --x2 
  else vertCollision p offs bs
    where 
      (x1,y1)= topLt (floorBox block)
      (x2,y2)= bottomRt (floorBox block)
      (pRx,pRy)= topRt(hitBox p)
      (pLx,pLy)= bottomLt (hitBox p)
      --low high
      collidingL = inBetween pRx (x1-offs) (x2-offs) && inBetween pRy (y2) (y1)
      collidingR = inBetween pLx (x1-offs) (x2-offs)
      newBox x= newHitBox x (yPos p) (facingRight p) 
--newHitBox:: Float-> Float-> Bool-> HitBox    

horzCollision :: Player -> Float->[JBlock] -> Player
horzCollision p _ [] = if not (inAir p) then  p {inAir =True} else p
horzCollision p offs (block:bs) = 
  -- These numbers are NOT choosen arbituarly
  -- the 2* is for the scalar of the block
  -- (-50) is how many pixels off the left side of the block picture doesnt take up
  -- (-22) is how many pixels the right side of the block picture doesnt take up
  --If you change this, it will break collison in its current form
  if isCollision &&  ((yVel p) < 0.25) -- 0.25 b/c 0 resulted in jumping bug
    -- falling onto a block
    then p {yPos = ((2)*y1+85), inAir = False, yVel = 0} 
  else if isCollision && ((yVel p ) > 0.25) -- 0.25 b/c 0 resulted in jumping bug
    -- hitting head on block
    then p {yPos = (2*y2-25), inAir = True, yVel = 0} -- Somewhat arbitrary -25 (found via guess and check)
  else horzCollision p offs bs
    -- (x1, y1) is bottom left corner (min values)
    -- (x2, y2) is top right (max values)
    where 
      isCollision = inBlock block (xPos p, yPos p) offs 
      (x1, y2) = topLeft block
      y1 = y2 - height block
      
inBlock :: JBlock -> (Float, Float) -> Float-> Bool
-- These numbers are NOT choosen arbituarly
-- the 2* is for the scalar of the block
-- (-50) is how many pixels off the left side of the block picture doesnt take up
-- (-22) is how many pixels the right side of the block picture doesnt take up
--If you change this, it will break collison in its current form
-- Joel's Note: 2*y2 + 80, where 80 was found by guess and check
inBlock block (x, y) offs = inBetween x (x1-offs) (x2-offs) && inBetween y y1 (y2+100) --TODO make this not hard-coded :(  x1-25.5), y1), ((x2-18.5)
  -- (x1, y1) is bottom left corner (min values)
  -- (x2, y2) is top right (max values)
  where 
    (x1, y2) = topLt (floorBox block)
    (x2,y1) = bottomRt (floorBox block)

--This doesnt work anymore because of the hitboxes
handleFall::Player->Player
handleFall p =
  if health p >0 &&  yPos p <(-500) 
  then p{health= (health p) -1, yPos =500, hitBox=(newHitBox (xPos p) 500 (facingRight p)),yVel=(0)} 
  else p
 
inBetween :: Float -> Float -> Float -> Bool
inBetween x low high = x > low && x < high



