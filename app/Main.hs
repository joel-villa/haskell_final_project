{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
--import Data.Text 
-- import Brillo.Interface.IO.Game (Event (EventKey))
import Types
import EventHandler
import Tick

-- | Display the last event received as text.
--prelude.show.event, very helpful

--worldToPicture' World -> Picture->Picture->Picture 


main :: IO ()
main =do
  bmp <- loadBMP "resources/Sheep.bmp"
  floorbmp <- loadBMP "resources/pinkGrass.bmp"
  clouds <- loadBMP "resources/patchOfClouds.bmp"

  play
    (InWindow "GameEvent" (1000, 900) (0,0))   --Display mode
    ((makeColor 0.75 0.75 1 0.5))    --Background color.
    100  --Number of simulation steps to take for each second of real time.
    (World starterSheep (Level (makeTup (-200.0) 200.0 10.5) [(20,80)]) 0)   --The initial world.
    (\world -> (pictures (worldToPicture world bmp floorbmp clouds))) --A function to convert the world a picture.
    handleEvent   -- (Event -> world -> world) A function to handle input events.
    tick --(Float -> world -> world)



makeTup :: Float-> Float->Float->[(Float,Float)]
makeTup start end step
  |start > end =[((start-(5.0*step)),-24),((-start+(3.0*step)),-24),((start-(18.0*step)),-24),((-start+(11.0*step)),-24)] 
  |otherwise =[(start+step,(-30))] ++ makeTup (start+step) end step

{-
-- data World = World { pos  :: (Float,Float)   }     -- current level depth}  -- all levels
worldToPicture :: World -> Picture->Picture->Picture-> [Picture]
worldToPicture (World(x,y) (Level [] [(b,c)]) offset) pic floorpic clouds= 
  Translate (20*(x-offset)) (20*y) (pic) :[Scale 2.5 2.5 (Translate b c clouds)]
worldToPicture (World(x,y) (Level ((z,w):fs) xs) offset) pic floorpic clouds = 
  scale 2 2 (Translate (5*(z-offset)) (5*w) (floorpic)) : worldToPicture (World(x,y) (Level fs xs) offset) pic floorpic clouds


worldToPicture:: :: World -> Picture->Picture->Picture-> [Picture]
worldToPicture wld@(World(x,y) (Level _ [(b,c)]) _) pic floorpic clouds =
  (Translate (20*(x-offset)) (20*y) (pic)): (Scale 2.5 2.5 (Translate b c clouds)):drawFloor world floorpic

drawFloor :: World -> Picture -> [Picture]
drawFloor (World(x,y) (Level [] _) _) floorpic = []
drawFloor (World(x,y) (Level ((z,w):fs) xs) offset) floorpic =
  scale 2 2 (Translate (5*(z-offset)) (5*w) (floorpic)) : drawFloor (World(x,y) (Level ((z,w):fs) xs) offset) floorpic
-}

worldToPicture :: World -> Picture->Picture->Picture-> [Picture]
-- worldToPicture w sheep floor cloud = Scale 2.5 2.5 (Translate (b-offset) c clouds):Scale 2 1.5 (Translate ((b*(-18))-offset) (c*3) clouds) :drawFloor w floorpic ++[drawPlayer w pic]
--   where 
--     level =
worldToPicture w@(World plr (Level _ [(b,c)]) offset) pic floorpic clouds =
  Scale 2.5 2.5 (Translate (b-offset) c clouds):Scale 2 1.5 (Translate ((b*(-18))-offset) (c*3) clouds) :drawFloor w floorpic ++[drawPlayer w pic]

drawFloor :: World -> Picture -> [Picture]
drawFloor (World _ (Level [] _) _) floorpic = []
drawFloor (World plr (Level ((z,w):fs) xs) offset) floorpic =
  scale 2 2 (Translate (5*(z-offset)) (5*w) (floorpic)) : drawFloor (World plr (Level fs xs) offset) floorpic

drawPlayer :: World -> Picture -> Picture
drawPlayer world pic = Translate (20*(xPos (hero world)-(offset world))) (20*(yPos(hero world))) (pic)



