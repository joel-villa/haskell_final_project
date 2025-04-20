module EventHandler where
import Types
import Brillo.Interface.IO.Interact


newHandleEvent :: Event ->World ->World 
newHandleEvent(EventKey (SpecialKey KeyRight) Down _ _) w =w{hero= ((hero w){xVel=10})}
newHandleEvent(EventKey (SpecialKey KeyRight) Up _ _) w =w{hero= ((hero w){xVel=0})}
newHandleEvent(EventKey (SpecialKey KeyLeft) Down _ _) w =w{hero= ((hero w){xVel=(-5)})}
newHandleEvent(EventKey (SpecialKey KeyLeft) Up _ _) w =w{hero= ((hero w){xVel=(0)})}
newHandleEvent(EventKey (SpecialKey KeyUp) Down _ _) w =w{hero= (handleJump (hero w))}
newHandleEvent(EventKey (SpecialKey KeyUp) Up _ _) w =w{hero= ((hero w){yVel=0})}
newHandleEvent _ w=w

handleJump :: Player ->Player
handleJump plr 
  |inAir plr = plr 
  |otherwise = plr{yVel=20, inAir=True}

