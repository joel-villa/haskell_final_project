{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
--import Data.Text 
import Brillo.Interface.IO.Game (Event (EventKey))
import Brillo.Interface.IO.Interact


-- | Display the last event received as text.
--prelude.show.event, very helpful
main :: IO ()

main =do
  picture <- loadBMP "357BMPstuff/LilDude.bmp"
  play
    (InWindow "GameEvent" (700, 100) (10, 10))   --Display mode
    white    --Background color.
    100  --Number of simulation steps to take for each second of real time.
    (World(0,0))   --The initial world.
    (\(World(x,y)) -> (Translate (20*x) (20*y) (picture)) )  --A function to convert the world a picture.
    (\event world ->  handleEvent event world)   -- (Event -> world -> world) A function to handle input events.
    (\_ world -> world) --(Float -> world -> world)
    --Translate (50*t) (0 then (Translate (50*t) (0) (picture)))

{-
main = do 
    picture <- loadBMP "MachoMan.bmp"
    display (InWindow "Pic" (500, 500)(10,10)) white picture
-}

data World = World { pos  :: (Float,Float)   }     -- current level depth}  -- all levels

handleEvent :: Event ->World-> World
handleEvent(EventKey (SpecialKey KeyRight) _ _ _) (World(x,y))=(World(x+1,y))
handleEvent(EventKey (SpecialKey KeyLeft) _ _ _) (World(x,y))=(World(x-1,y))
handleEvent(EventKey (SpecialKey KeyUp) _ _ _) (World(x,y))=(World(x,y+1))
handleEvent(EventKey (SpecialKey KeyDown) _ _ _) (World(x,y))=(World(x,y-1))
handleEvent e (World(x,y))=(World(x,y))

worldToPicture:: World-> String
worldToPicture (World (0,0)) ="@"
worldToPicture (World (x,y)) = "\n " ++ worldToPicture (World (x-1,y-1))
{-
handleEvent:: Event->world ->world
handleEvent event world=case(event) of
    "(EventKey (SpecialKey KeyRight) Down"-> "You are moving down"

-}

    
 
{-
playSource#

:: Display	
Display mode.

-> Color	
Background color.

-> Int	
Number of simulation steps to take for each second of real time.

-> world	
The initial world.

-> (world -> Picture)	
A function to convert the world a picture.

-> (Event -> world -> world)	
A function to handle input events.

-> (Float -> world -> world)	
A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

-> IO ()	 
Play a game in a window. Like simulate, but you manage your own input events.
-}