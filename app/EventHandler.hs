module EventHandler where
import Types
import Brillo.Interface.IO.Interact

handleEvent :: Event ->World-> World
handleEvent(EventKey (SpecialKey KeyRight) _ _ _) (World(x,y))=(World(x+1,y))
handleEvent(EventKey (SpecialKey KeyLeft) _ _ _) (World(x,y))=(World(x-1,y))
handleEvent(EventKey (SpecialKey KeyUp) _ _ _) (World(x,y))=(World(x,y+1))
handleEvent(EventKey (SpecialKey KeyDown) _ _ _) (World(x,y))=(World(x,y-1))
handleEvent e (World(x,y))=(World(x,y)) -- Other cases, do nothing 