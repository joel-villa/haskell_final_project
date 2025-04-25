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

main :: IO ()
main =do
  sheep <- loadBMP "resources/Sheep.bmp"
  floorbmp <- loadBMP "resources/pinkGrass.bmp"
  clouds <- loadBMP "resources/patchOfClouds.bmp"
  angelGuy <-loadBMP "resources/angleDude.bmp"
  heart<-loadBMP "resources/heart.bmp"
  sheepSwing <- loadBMP "resources/SheepSwing.bmp"
  sheepLeft <- loadBMP "resources/SheepLeft.bmp"
  sheepLeftSwing <- loadBMP "resources/SheepLeftSwing.bmp"

  let bmpList = [floorbmp, sheep, clouds, angelGuy, heart, sheepSwing, 
                 sheepLeft, sheepLeftSwing]
  play
    (InWindow "GameEvent" (1000, 900) (0,0))   --Display mode
    backgroundColor                            -- in Init.hs
    fps                                        -- in Init.hs
    initWorld                                  -- in Init.hs
    (\world -> (worldToPicture world bmpList)) --A function to convert the world a picture.
    newHandleEvent                                -- in EventHandler.hs
    tick                                       -- in Tick.hs

worldToPicture:: World -> [Picture]->Picture
worldToPicture w pics = 
  pictures((drawPlayer h offset' pPic) : (pictures (drawFloor bs offset' (pics!!0))) :drawIntro w: (drawHeart (pics!!4) w)++ (drawExtras w (pics!!2))++drawEnimies bgs offset' (pics!!3)) 
  where 
    offset' = (offset w)      -- The offset of the world
    bs = terrain (curLevel w) -- The JBlocks of this level
    h = hero w                -- current player info
    bgs = enemies w           -- baddies
    drawExtras w pic =(Scale 2.5 2.5 (Translate (0) 100 pic)):[Scale 1.5 1 (Translate (-300) 300 pic)]     -- Sorry I will fix this later
    pPic = getPlayPic h pics  -- allows for multiple player pictures

getPlayPic :: Player -> [Picture] -> Picture
getPlayPic p pics = 
  if active (weapon p) 
    then case facingEast of 
      True -> pics !! 5  -- Swinging to right
      False -> pics !! 7 -- swinging to left
  else 
    case facingEast of
       True -> pics !! 1  -- facing right
       False -> pics !! 6 -- facing left
  where 
    facingEast = facingRight p


--Draws multiple enimies but in the current form, with only one picture
drawEnimies :: [BadGuy]->Float->Picture->[Picture]
drawEnimies [] _ _ =[]
drawEnimies (bg:bgs) offs pic = (Scale 2 2 (Translate (x0-offs) y0 pic)) : hitBox : drawEnimies bgs offs pic
  where 
    x0 = x (pathing bg)
    y0=y (pathing bg)
    -- hitCircle's feel more natural w/ Brillo (since x,y is center of pic)
    -- hitCircle1 = scale 2 2 (translate (x1 - offs) y1 (circle (hitRadius bg)))
    xCenter = (x0 - offs)*2
    yCenter = (y0*2)
    hitR = hitRadius bg
    (x1, y1) = (xCenter - hitR, yCenter + hitR)
    (x2, y2) = (xCenter + hitR, yCenter - hitR)
    pts = [(x1, y1), (x1, y2), (x2, y2), (x2, y1), (x1, y1)]
    hitBox = (line pts)
    -- hitCircle = (translate xCenter yCenter (circle (hitRadius bg))) 

-- Draws the hearts, this should stay consistent throughout any level
drawHeart :: Picture -> World ->[Picture]
drawHeart pic w= go (health (hero w)) pic 
  where 
    go 0.0 _ =[]
    go n pic =  (Translate ((n*80)+170) 380 pic) :go (n-1) pic

-- draws the text introduction
drawIntro :: World ->Picture
drawIntro w = draw x
  where
    num =intro w
    x= if (num `div` 150) >5 then 5 else (num `div` 150)
    draw x = Color red (Translate (-450- (offset w)) 0 (scale 0.65 1 (text ((inTheBegining w)!!x))))
    
--draws player, with the offset
drawPlayer :: Player -> Float ->  Picture -> Picture
drawPlayer h offs pic = pictures [translate x y pic, playerPos, weaponHBox] 
  where
    y = yPos h
    x = (xPos h) - offs
    playerPos = translate x y (circle (10)) -- arbitrary 10 (for player position)
    w = weapon h -- The weapon
    (relX,relY) = relativePos w  -- The weapon's position relative to sheep
    w_hit_rad = wDamageRadius w
    (wCenterX, wCenterY) = (x + relX, y + relY)
    (wx1, wy1) = (wCenterX - w_hit_rad, wCenterY + w_hit_rad)
    (wx2, wy2) = (wCenterX + w_hit_rad, wCenterY -w_hit_rad)
    pts = [(wx1, wy1), (wx2, wy1), (wx2, wy2), (wx1, wy2), (wx1, wy1)]
    weaponHBox = line pts
-- drawPlayer world pic = Translate (20*((xPos (hero world))-(getOffset (xPos (hero world))))) ((yPos(hero world))) (pic)

--(Line [((2*x1-50), ((2)*y1)), ((2*x2-22), (2*y2))]) :  -- these nums are NOT choosen arbitrarely
                              --(Line[((2*x0-50), ((2)*y1)), ((2*(x0+w)-22), (2*y2))]):
                              -- the 2* is for the scalar of the block
                              -- (-50) is how many pixels off the left side of the block picture doesnt take up
                              -- (-22) is how many pixels the right side of the block picture doesnt take up
                              --If you change this, it will break collison in its current form
drawFloor :: [JBlock] -> Float -> Picture -> [Picture] 
drawFloor [] offs pic = []
drawFloor (b:bs) offs pic = floorPic:drawFloor bs offs pic                            
  where 
    (x0, y1) = topLeft b
    x1 = x0 - offs
    w = width b
    h = height b
    x2 = x1 + w
    y2 = y1 - h
    floorPic = (scale 2 2(translate x1 y1  pic))

--the block is actually -25 on left -21 on right so 54 pixels