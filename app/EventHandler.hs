module EventHandler where
import Types
import Brillo.Interface.IO.Interact


newHandleEvent :: Event ->World ->World 
newHandleEvent(EventKey (Char 'd') Down _ _) w = w{hero = (handleRight (hero w))}
newHandleEvent(EventKey (Char 'd') Up   _ _) w = w{hero = ((hero w){xVel = 0})}
newHandleEvent(EventKey (Char 'a') Down _ _) w = w{hero = (handleLeft (hero w))}
newHandleEvent(EventKey (Char 'a') Up   _ _) w = w{hero = ((hero w){xVel = (0)})}
newHandleEvent(EventKey (Char 'w') Down _ _) w = w{hero = (handleJump (hero w))}
newHandleEvent(EventKey (SpecialKey KeySpace) Down _ _) w = w{hero = (useWeapon (hero w))}
newHandleEvent (EventKey (Char 'e') Down _ _) w =w{hero = (sheepBasedMagic (hero w))}
newHandleEvent _ w = w

-- handle left user input, returning the new player
handleLeft :: Player -> Player
handleLeft p = p {xVel = -7, facingRight = False, weapon = newWeapon}
  where
    --updating weapon hit-box
    newWeapon = (weapon p) {relativePos = (-15, -25)}

-- handle right user input, returning the new player
handleRight :: Player -> Player
handleRight p = p {xVel = 7, facingRight = True, weapon = newWeapon}
  where
    --updating weapon hit-box
    newWeapon = (weapon p) {relativePos = (15, -25)}
-- handle jump user input, returning the new player
handleJump :: Player ->Player
handleJump plr 
  |inAir plr = plr 
  |otherwise = plr{yVel=17, inAir=True}

-- Handle use weapon input, returning the new player
useWeapon :: Player -> Player
useWeapon p = p {weapon = swungSword}
  where
    oldWeapon = weapon p 
    swungSword = oldWeapon {wVelocity = 8, active = True} -- This constant determines duraton of swing

sheepBasedMagic:: Player->Player
sheepBasedMagic p= p{magic=newMagic}
  where
    newMagic = Projectiles{projBox=newBox,durration=40,direction=newVel}
    newVel = if facingRight p then 7 else (-7)
    newBox=makeHitbox x y 20 30
    x=(x1+x2)/2
    y=(y1 )
    (x1,y1)=(topLt (hitBox p))
    (x2,y2)=(bottomRt (hitBox p))
