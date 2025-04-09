module WorldToPic where
import Types
import Brillo

-- getPicture :: IO Picture -> Picture

worldToPicture :: World -> Picture-> Picture
worldToPicture (World(x,y)) pic = Translate (20*x) (20*y) (pic) 
--TODO make this capable of being passed all resources? Maybe should be moved to Main?


worldToPicture':: World -> String
worldToPicture' (World (0,0)) ="@"
worldToPicture' (World (x,y)) = "\n " ++ worldToPicture' (World (x-1,y-1))