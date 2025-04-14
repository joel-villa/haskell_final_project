module Types where

--Avalible directions, maybe what to derive show or eq
-- Up is more like jump, so migh be a little more difficult to associate
--data Directions = Right | Left | Up | Down

--A sheep fellow, maybe he will be purple, maybe he will be green
data Player= 
  Player { 
    xPos   :: Float, 
    yPos   :: Float, 
    xVel   :: Float,
    yVel   :: Float,
    health :: Int,
    inAir  :: Bool,
    money  :: Int,
    sMoneyAndSValubles :: [Item]
  }

-- Should be the same as player but w/out fallingTime?? and yVel 
data BadGuy = 
  BadGuy {
    health_bad :: Int, 
    pos_bad    :: (Float, Float), -- (x,y) ?  
    money_bad  :: Int, 
    pouch      :: [Item], 
    xVel_bad   :: Float
  }

data Item = 
  Potion {
    quantEffect :: Int,  -- Like how much it increases your health/ xvel/yvel
    descriptor  :: String, --Exact descriptor, this would be for the users benefit
    effect      :: Effect 
  }     -- diff effect
  | Weapon {
    wDamage :: Int,       -- added to dmg rolls on hits
    wDesc   :: String     --Exact descriptor, this would be for the users benefit
  }   
-- Maybe a armor thing?

data InvintorAction = DrinkPotion | EquipArmor | EquipWeapon

--          x/yvel ++  health++   health--   yVel--
data Effect = SpeedUp| Healing | Harming | SlowFalling   -- Ignore I stole this from minecwaft


data Level = 
  Level {
    floorpos :: [(Float,Float)], 
    extras   :: [(Float,Float)]
    } 
data World = 
  World { 
    hero     :: Player, 
    curLevel :: Level, 
    offset   :: Float
    } -- TODO change this to other World