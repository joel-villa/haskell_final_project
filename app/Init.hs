module Init where
import Brillo
import Types
import FirstWorld

-- fps: Number of simulation steps to take for each second of real time
fps :: Int 
fps = 60

-- The background color: 
backgroundColor :: Color
backgroundColor = (makeColor 0.75 0.75 1 0.5)


initWorld :: World
--initWorld = World starterSheep levelOne 0
-- initWorld = World starterSheep (Level (makeTup (-200.0) 200.0 10.5) [(20,80)]) 0
initWorld = World starterSheep(firstWorldToLevel(firstWorld (-20) (-30.0) 10.5 5.5)) 0
l{-evelOne :: Level
levelOne = Level terrain extras baddies
  where 
    terrain = makeBlocks (-200, -30) 200 10.5 10.5
    extras = [(20, 80)]
    baddies = []
      -}

makeBlocks :: (Float, Float) -> Float -> Float -> Float -> [Block] -- make horizontal path of blocks
makeBlocks (initX, initY) endX h w  
  | initX > endX = []
  | otherwise = Block (initX, initY) h w None : makeBlocks (initX + w, initY) endX h w
makeTup :: Float-> Float->Float->[(Float,Float)]
makeTup start end step
  |start > end =[((start-(5.0*step)),-24),((-start+(3.0*step)),-24),((start-(18.0*step)),-24),((-start+(11.0*step)),-24)] 
  |otherwise =[(start+step,(-30))] ++ makeTup (start+step) end step


-- Your classic begener sheep 
starterSheep :: Player
starterSheep =
  Player{
    xPos   = 0,
    yPos   = (-237),
    xVel   = 0,
    yVel   = 0,
    health = 100, 
    inAir  = False,
    money  = 10, 
    sMoneyAndSValubles = [Potion {quantEffect=10,descriptor="Health Potion +10", effect=Healing}]
  }