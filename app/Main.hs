{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
import Types
import EventHandler
import Tick
import Init

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
    (backgroundColor)    --Background color, in Init.hs
    100  --Number of simulation steps to take for each second of real time.
    (World starterSheep (Level (makeTup (-200.0) 200.0 10.5) [(20,80)]) 0)   --The initial world.
    (\world -> (worldToPicture world [bmp, floorbmp, clouds])) --A function to convert the world a picture.
    handleEvent   -- (Event -> world -> world) A function to handle input events.
    tick --(Float -> world -> world)



makeTup :: Float-> Float->Float->[(Float,Float)]
makeTup start end step
  |start > end =[((start-(5.0*step)),-24),((-start+(3.0*step)),-24),((start-(18.0*step)),-24),((-start+(11.0*step)),-24)] 
  |otherwise =[(start+step,(-30))] ++ makeTup (start+step) end step


worldToPicture:: World -> [Picture]->Picture
worldToPicture w pics = pictures((drawPlayer w (pics!!1)):(drawFloor w (pics!!0)) ++ (drawExtras w (pics!!2)))
  where 
    drawExtras w pic =(Scale 2.5 2.5 (Translate (-50) 50 pic)):[Scale 2 1.5 (Translate (-200) 100 pic)]     -- Sorry I will fix this later

drawPlayer :: World -> Picture -> Picture
drawPlayer world pic = Translate (20*(xPos (hero world)-(offset world))) (20*(yPos(hero world))) (pic)

drawFloor :: World->Picture -> [Picture]
drawFloor w pic   = case (floorpos(curLevel w)) of 
  [] -> []
  ((x,y):fs) -> floorPic x y w : drawFloor w{curLevel= ((curLevel w){floorpos=fs})} pic
  where
    floorPic x y w =scale 2 2 (Translate (5*(x-(offset w))) (5*y) (pic))  -- Draws the floor, scales each tile by 2X2







