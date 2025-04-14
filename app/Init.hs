module Init where
import Brillo
import Types

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