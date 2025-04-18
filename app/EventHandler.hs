module EventHandler where
import Types
import Brillo.Interface.IO.Interact


newHandleEvent :: Event ->World ->World 
newHandleEvent(EventKey (SpecialKey KeyRight) Down _ _) w =w{hero= ((hero w){xVel=1})}
newHandleEvent(EventKey (SpecialKey KeyRight) Up _ _) w =w{hero= ((hero w){xVel=0})}
newHandleEvent(EventKey (SpecialKey KeyLeft) Down _ _) w =w{hero= ((hero w){xVel=(-1)})}
newHandleEvent(EventKey (SpecialKey KeyLeft) Up _ _) w =w{hero= ((hero w){xVel=(0)})}
newHandleEvent(EventKey (SpecialKey KeyUp) Down _ _) w =w{hero= (handleJump (hero w))}
newHandleEvent(EventKey (SpecialKey KeyUp) Up _ _) w =w{hero= ((hero w){yVel=0})}
newHandleEvent _ w=w

handleJump :: Player ->Player
handleJump plr 
  |inAir plr = plr 
  |otherwise = plr{yVel=15, inAir=True}

handleEvent :: Event ->World-> World
handleEvent(EventKey (SpecialKey KeyRight) _ _ _) world=updateXPos 1 world
handleEvent(EventKey (SpecialKey KeyLeft) _ _ _) world=updateXPos (-1) world
handleEvent(EventKey (SpecialKey KeyUp) _ _ _) world=updateYPos (1) world
handleEvent(EventKey (SpecialKey KeyDown) _ _ _) world=updateYPos (-1) world
handleEvent _ world=world -- Other cases, do nothing 


handleVertCollision::Player -> [(Float,Float)]-> Float->Player 
handleVertCollision sheep floor yDisp = undefined


getOffset :: Float -> Float 
getOffset x
  |x > 5     = (x - 5)
  |x < (-5)  = (x + 5)
  |otherwise = 0

updateXPos :: Float -> World -> World
updateXPos n w = w {hero = updatedHero, offset= getOffset (xPos updatedHero)}
  where
    oldHero = hero w                                  -- get old hero 
    updatedHero = oldHero {xPos = (xPos oldHero) + n} -- update position of hero

updateYPos :: Float -> World -> World
updateYPos n w = w {hero = updatedHero,offset= getOffset (xPos updatedHero)}
  where
    oldHero = hero w                                  -- get current hero from w
    updatedHero = oldHero {yPos = (yPos oldHero) + n} -- updated yPosition

{-
TODO is the needed still? 
updatePlayer :: Player -> Float -> Float -> Player
updatePlayer (Player x y h ft m ms) z q = (Player z q h ft m ms)
-}


{-
bonk:: World->Player ->Bool 
bonk =undefined

contains :: (Float, Float)->[(Float,Float)]
contains (x,y) (z,q):fs 
  |(abs (x-z))< (.5) =False
  |(abs (y-q))<(.5) =False
--(+1)
canGoRight:: (Float,Float)->[(Float,Float)]
canGoRight (x,y) fs= foldr(\(w,q) acc ->if ((abs (x+1)-w)<.2) and (y==q) then false else True and acc)
-}
