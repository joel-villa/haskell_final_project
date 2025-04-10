module EventHandler where
import Types
import Brillo.Interface.IO.Interact

handleEvent :: Event ->World-> World
handleEvent(EventKey (SpecialKey KeyRight) _ _ _) (World(x,y)fs off )=(World(x+1,y) fs (getOffset (x+1)))
handleEvent(EventKey (SpecialKey KeyLeft) _ _ _) (World(x,y) fs off)=(World(x-1,y) fs (getOffset (x-1)))
handleEvent(EventKey (SpecialKey KeyUp) _ _ _) (World(x,y) fs off)=(World(x,y+1) fs (getOffset (x)))
handleEvent(EventKey (SpecialKey KeyDown) _ _ _) (World(x,y) fs off)=(World(x,y-1) fs (getOffset x))
handleEvent e (World(x,y) fs off)=(World(x,y) fs (getOffset (x))) -- Other cases, do nothing 

getOffset :: Float -> Float 
getOffset x
  |x>5 = (x-5)
  |x<(-5) = (x+5)
  |otherwise =0