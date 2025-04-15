module FirstWorld where
import Types
firstWorld :: [Terrain]
firstWorld = [ Block 0 0, Block 1 0,Block 2 0, Block 3 0, Block 4 0,Block 4 3, Block 5 3, Block 5 0, Block 6 0, Block 7 0, Block 8 0, Block  8 1]

firstWorldToLevel :: [Terrain] -> Level
firstWorldToLevel [] = []
firstWorldToLevel (Block x y : xs) = Level ((x,y) : floorpos( firstWorldToLevel xs))
firstWorldToLevel (Cloud x y v : xs) = Level ((x,y) : extras(firstWorldToLevel xs))