{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
import Types
import EventHandler
import Tick
import Init
import GHC.Float

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
    backgroundColor                            -- in Init.hs
    fps                                        -- in Init.hs
    initWorld                                  -- in Init.hs
    (\world -> (worldToPicture world [floorbmp,bmp, clouds])) --A function to convert the world a picture.
    newHandleEvent                                -- in EventHandler.hs
    tick                                       -- in Tick.hs

worldToPicture:: World -> [Picture]->Picture
worldToPicture w pics = pictures((drawPlayer h offset (pics!!1)) : (pictures (drawFloor bs offset (pics!!0))) : (drawExtras w (pics!!2)))
  where 
    offset = getOffset (xPos (hero w))
    bs = terrain (curLevel w)
    h = hero w 
    drawExtras w pic =(Scale 2.5 2.5 (Translate (-(getOffset (xPos (hero w)))) 100 pic)):[Scale 1.5 1 (Translate (-300-(getOffset (xPos (hero w)))) 300 pic)]     -- Sorry I will fix this later

drawPlayer :: Player -> Float ->  Picture -> Picture
drawPlayer h offset pic = pictures [translate x y pic, translate x y (circle 5)] --arbitrary 5, center is position of player
  where
    x0 = xPos h
    y = yPos h
    x = x0 - offset
-- drawPlayer world pic = Translate (20*((xPos (hero world))-(getOffset (xPos (hero world))))) ((yPos(hero world))) (pic)

drawFloor :: [JBlock] -> Float -> Picture -> [Picture] 
drawFloor [] offset pic = []
drawFloor (b:bs) offset pic = floorPic                     : 
                              (translate x1 y1 (circle 3)) :  --These circles are the corners of the JBlock?
                              (translate x2 y1 (circle 3)) :  -- 3 is arbitrary
                              (translate x2 y2 (circle 3)) : 
                              (translate x1 y2 (circle 3)) : 
                              drawFloor bs offset pic
  where 
    (x0, y1) = topLeft b
    x1 = x0 - offset
    w = width b
    h = height b
    x2 = x1 + w
    y2 = y1 - h
    -- points = [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
    floorPic = translate x1 y1 pic
    -- rectPic = translate x1 y1 (polygon points) -- Where the block actually is? 

-- drawFloor :: World->Picture -> [Picture]
-- drawFloor w pic = case (terrain(curLevel w)) of 
--   [] -> []
--   ((JBlock (x,y) _ _ _):bs) -> floorPic x y w : drawFloor w{curLevel= ((curLevel w){terrain=bs})} pic
--   where
--     floorPic x y w =scale 2 2 (Translate ((x-(getOffset (xPos (hero w))))) (y) (pic))  -- Draws the floor, scales each tile by 2X2







