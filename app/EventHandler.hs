module EventHandler where
import Types
import Brillo.Interface.IO.Interact


newHandleEvent :: Event ->World ->World 
newHandleEvent(EventKey (Char 'd') Down _ _) w =w{hero= ((hero w){xVel=10})}
newHandleEvent(EventKey (Char 'd') Up   _ _) w =w{hero= ((hero w){xVel=0})}
newHandleEvent(EventKey (Char 'a') Down _ _) w =w{hero= ((hero w){xVel=(-5)})}
newHandleEvent(EventKey (Char 'a') Up   _ _) w =w{hero= ((hero w){xVel=(0)})}
newHandleEvent(EventKey (Char 'w') Down _ _) w =w{hero= (handleJump (hero w))}
newHandleEvent(EventKey (Char 'w') Up   _ _) w =w{hero= ((hero w){yVel=0})}
newHandleEvent(EventKey (Char 'x') Down _ _) w = w{hero = (useWeapon (hero w))}
newHandleEvent _ w = w

handleJump :: Player ->Player
handleJump plr 
  |inAir plr = plr 
  |otherwise = plr{yVel=20, inAir=True}

-- Handle use weapon input
useWeapon :: Player -> Player
useWeapon p = p {weapon = swungSword}
  where
    oldWeapon = weapon p 
    swungSword = oldWeapon {wVelocity = 8} -- This constant determines duraton of swing