module Init where
import Brillo
import Types

-- The background color: 
backgroundColor :: Color
backgroundColor = (makeColor 0.75 0.75 1 0.5)

-- Your classic begener sheep 
starterSheep =
  Player{
    xPos =0,
    yPos =(-12),
    health=100, 
    fallingTime=0,
    money=10, 
    sMoneyAndSValubles=[Potion {quantEffect=10,descriptor="Health Potion +10", effect=Healing}]
  }