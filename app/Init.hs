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
initWorld = World starterSheep(firstWorldToLevel(firstWorld (-200) (-160.0) 51 (30))) 0 0 heavenIntro [angel,angel{pathing=basicAngelPath2},angel{pathing=basicAngelPath3}]

makeBlocks :: (Float, Float) -> Float -> Float -> Float -> [JBlock] -- make horizontal path of blocks
makeBlocks (initX, initY) endX h w  
  | initX > endX = []
  | otherwise = JBlock (initX, initY) h w None : makeBlocks (initX + w, initY) endX h w
makeTup :: Float-> Float->Float->[(Float,Float)]
makeTup start end step
  |start > end =[((start-(5.0*step)),-24),((-start+(3.0*step)),-24),((start-(18.0*step)),-24),((-start+(11.0*step)),-24)] 
  |otherwise =[(start+step,(-30))] ++ makeTup (start+step) end step


-- Your classic begener sheep 
starterSheep :: Player
starterSheep =
  Player{
    xPos   = 0,
    yPos   = (-175),
    xVel   = 0,
    yVel   = 0,
    health = 3, 
    inAir  = False,
    money  = 10, 
    sMoneyAndSValubles = [Potion {quantEffect=10,descriptor="Health Potion +10", effect=Healing}]
  }

angel ::BadGuy
angel=
  BadGuy{
    health_bad=10,
    money_bad=10,
    pouch=[],
    pathing= basicAngelPath
}
basicAngelPath= 
  JPath{
    initPos=(200,(-100)),
    goalPos =(-100,(-100)),
    x=100,
    y=(-100),
    xVelocity=(-0.5),
    yVelocity=0
  }

basicAngelPath2= 
  JPath{
    initPos=(300,(0)),
    goalPos =(600,(300)),
    x=300,
    y=(0),
    xVelocity=(0.5),
    yVelocity=(0.5)
  }

basicAngelPath3= 
  JPath{
    initPos=(900,(0)),
    goalPos =(1200,(0)),
    x=900,
    y=(0),
    xVelocity=(0.5),
    yVelocity=(0)
  }

