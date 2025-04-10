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
  let level1 = Level {floorPos= (makeTup (-100.0) 100.0 10.5), floorString = "resources/pinkGrass.bmp",extraPos =[(20.0,70.0)],extraString=["resources/patchOfClouds.bmp"] }
  let starterWorld= World ((0,(-12)) level1 0)
  bmp <- loadBMP "resources/Sheep.bmp"
  floorbmp <- loadBMP "resources/pinkGrass.bmp"
  clouds <- loadBMP "resources/patchOfClouds.bmp"

  -- TODO make this able to pass all resources
  

  play
    (InWindow "GameEvent" (1000, 1000) (10, 10))   --Display mode
    ((makeColor 0.75 0.75 1 0.5))    --Background color.
    100  --Number of simulation steps to take for each second of real time.
    (starterWorld)   --The initial world.
    (\world -> (pictures (worldToPicture world bmp floorbmp clouds))) --A function to convert the world a picture.
    handleEvent   -- (Event -> world -> world) A function to handle input events.
    tick --(Float -> world -> world)



makeTup :: Float-> Float->Float->[(Float,Float)]
makeTup start end step
  |start > end =[((start-(5.0*step)),-20),((-start+(3.0*step)),-20)] 
  |otherwise =[(start+step,(-30))] ++ makeTup (start+step) end step


-- data World = World { pos  :: (Float,Float)   }     -- current level depth}  -- all levels
worldToPicture :: World -> Picture->Picture->Picture-> [Picture]
worldToPicture (World(x,y) (Level [] _ _ _) offset) pic floorpic clouds= Translate (20*(x-offset)) (20*y) (pic)
worldToPicture (World(x,y) (Level ((z,w):fs) a b c) offset) pic floorpic clouds = scale 2 2 (Translate (5*(z-offset)) (5*w) (floorpic)) : (World(x,y) (Level (fs) a b c ) offset) pic floorpic clouds
--TODO make this capable of 

{-
if ((player.rect.right - offset_x >= WIDTH - scroll_area_width) and player.x_vel > 0) or (
                (player.rect.left - offset_x <= scroll_area_width) and player.x_vel < 0):
            offset_x += player.x_vel
-}

