module SecondWorld where

import Types

secondWorld :: Float -> Float -> Float -> Float -> [Terrain]
secondWorld x y xs ys = [ Block x y, Block (x+xs) y, Block (x+(2*xs)) y,
                         Block (x+(3*xs)) y,Block (x+(4*xs)) y,Block (x+(5*xs)) y,
                         Lava (x+(6*xs)) y False ,Lava (x+(7*xs)) y True, Lava (x+(8*xs)) y False,
                         Block (x+(9*xs)) y,Block (x+(10*xs)) y,Block (x+(11*xs)) y,
                         Block (x+(12*xs)) y, Block (x+(13*xs)) y, Lava (x+(14*xs)) y False,
                         Lava (x+(15*xs)) y True, Lava (x+(16*xs)) y False, Block (x+(17*xs)) y,
                         Block (x+(18*xs)) y,Block (x+(19*xs)) y,Block (x+(20*xs)) y,
                         Block (x+(22*xs)) (y+(3*ys)),Block (x+(23*xs)) (y+(3*ys)),Block (x+(24*xs)) (y+(3*ys)),
                         Block (x+(25*xs)) (y+(6*ys)),Block (x+(26*xs)) (y+(6*ys)),Block (x+(27*xs)) (y+(6*ys)),
                         Block (x+(28*xs)) (y+(3*ys)),Block (x+(29*xs)) (y+(3*ys)),Block (x+(30*xs)) (y+(3*ys)),
                         Block (x+(33*xs)) (y+(3*ys)),Block (x+(35*xs)) (y+(6*ys)),Block (x+(36*xs)) (y+(6*ys)),
                         Block (x+(38*xs)) (y+(3*ys)),Block (x+(40*xs)) (y+(6*ys)),Block (x+(42*xs)) (y+(3*ys)),
                         Block (x+(44*xs)) (y+(5*ys)),Block (x+(45*xs)) (y+(5*ys)),Block (x+(47*xs)) (y+(6*ys)),
                         Block (x+(48*xs)) (y+(6*ys)),Block (x+(49*xs)) (y+(3*ys)),Block (x+(50*xs)) (y+(3*ys))]

