module FirstWorld where
import Types
firstWorld :: Float -> Float -> Float -> Float -> [Terrain]
firstWorld x y xs ys= [ Block x y, Block (x+(xs)) y,Block (x+(2.0*xs)) y, Block (x+(3.0*xs)) y, 
                           Block (x+(4.0*xs)) y,Block (x+(5.0*xs)) (y +(ys + (2.0*ys))), Block (x+(6.0*xs)) (y +(ys + (2.0*ys))), 
                           Block (x+(5.0*xs)) y, Block (x+(6.0*xs)) y, Block (x+(7.0*xs)) y, Block (x+(8.0*xs)) y,
                           Block  (x+(8.0*xs)) (y +(ys)), Block (x+(8.0*xs)) (y +(2*ys)), Block (x+(9.0*xs)) (y +(2*ys)),
                           Block (x+(10.0*xs)) (y +(2*ys)),Block (x+(10.0*xs)) (y +(ys)),Block (x+(10.0*xs)) (y),
                           Block (x+(11.0*xs)) (y),Block (x+(12.0*xs)) (y), Block (x+(13.0*xs)) (y),
                           Block (x+(14.0*xs)) (y),Block (x+(14.0*xs)) (y+ys),Block (x+(14.0*xs)) (y+(ys*2)),
                           Block (x+(15.0*xs)) (y+(ys*2)),Block (x+(16.0*xs)) (y+(ys*2)), Block (x+(16.0*xs)) (y+ys),
                           Block (x+(16.0*xs)) (y),Block (x+(17.0*xs)) (y), Block (x+(18.0*xs)) (y),
                           Block (x+(19.0*xs)) (y),Block (x+(20.0*xs)) (y),Block (x+(20.0*xs)) (y-ys),
                           Block (x+(20.0*xs)) (y-(2*ys)), Block (x+(20.0*xs)) (y-(3*ys)),Block (x+(23.0*xs)) (y),
                           Block (x+(23.0*xs)) (y-(ys)),Block (x+(23.0*xs)) (y-(ys*2)),Block (x+(23.0*xs)) (y-(ys*3)),
                           Block (x+(24.0*xs)) (y),Block (x+(25.0*xs)) (y), Block (x+(26.0*xs)) (y),
                           Block (x+(26.0*xs)) (y+(ys*2)),Block (x+(25.0*xs)) (y+(ys*4)),Block (x+(26.0*xs)) (y+(ys*6)),
                           Block (x+(27.0*xs)) (y),Block (x+(28.0*xs)) (y),Block (x+(28.0*xs)) (y+(ys)),
                           Block (x+(28.0*xs)) (y+(ys*2)), Block (x+(28.0*xs)) (y+(ys*3)),Block (x+(28.0*xs)) (y+(ys*4)),
                           Block (x+(28.0*xs)) (y+(ys*5)),Block (x+(28.0*xs)) (y+(ys*6)),Block (x+(29.0*xs)) (y+(ys*6)),
                           Block (x+(30.0*xs)) (y+(ys*6)),Block (x+(31.0*xs)) (y+(ys*6)),Block (x+(32.0*xs)) (y+(ys*6))]

firstWorldToLevelBlock :: [Terrain] -> [JBlock]
firstWorldToLevelBlock [] = []
firstWorldToLevelBlock (Block x y : xs) =  reverse((JBlock (x, y) 10.5 40.5 None): firstWorldToLevelBlock xs)
firstWorldToLevelBlock (Cloud x y z :xs) = firstWorldToLevelBlock xs

firstWorldToLevelCloud :: [Terrain] -> [Terrain]
firstWorldToLevelCloud [] = []
firstWorldToLevelCloud (Cloud x y v: xs ) = reverse(Cloud {cxpos = x , cypos = y, cvel = v} : firstWorldToLevelCloud xs)
firstWorldToLevelCloud (Block x y : xs) = firstWorldToLevelCloud xs

firstWorldToLevel :: [Terrain] -> Level
firstWorldToLevel terrain = Level {terrain = (firstWorldToLevelBlock terrain) , clouds = (firstWorldToLevelCloud terrain)}

