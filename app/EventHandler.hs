module EventHandler where
import Types
import Brillo.Interface.IO.Interact

handleEvent :: Event ->World-> World
handleEvent(EventKey (SpecialKey KeyRight) _ _ _) (World(x,y)fs )=(World(x+1,y) fs)
handleEvent(EventKey (SpecialKey KeyLeft) _ _ _) (World(x,y) fs)=(World(x-1,y) fs)
handleEvent(EventKey (SpecialKey KeyUp) _ _ _) (World(x,y) fs)=(World(x,y+1) fs)
handleEvent(EventKey (SpecialKey KeyDown) _ _ _) (World(x,y) fs)=(World(x,y-1) fs)
handleEvent e (World(x,y) fs)=(World(x,y) fs) -- Other cases, do nothing 