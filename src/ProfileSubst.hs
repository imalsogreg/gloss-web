{-|
    Versions of the picture, animation, and simulation code to substitute in
    during profiling builds.  When profiling is turned on, we can't use the
    GHC API, so instead we directly build in the following implementations.

    These are intentionally chosen to be somewhat demanding, in hopes that
    we can get some good profiling data.
-}
module ProfileSubst (
    picture,
    animation,
    simulation
    )
    where

import Graphics.Gloss
import GlossAdapters

-----------------------------------------------------------------------

picture = kochSnowflake 5

kochSnowflake n = pictures [
    rotate   0 (translate 0 (-sqrt 3 * 100 / 6) (kochLine 100 n)),
    rotate 120 (translate 0 (-sqrt 3 * 100 / 6) (kochLine 100 n)),
    rotate 240 (translate 0 (-sqrt 3 * 100 / 6) (kochLine 100 n))
    ]

kochLine k 0 = line [(-k/2, 0), (k/2, 0) ]
kochLine k n = pictures [
    translate ( k/3) 0 (kochLine (k/3) (n-1)),
    translate (-k/3) 0 (kochLine (k/3) (n-1)),
    translate (-k/12) (-sqrt 3 * k/12) (rotate 300 (kochLine (k/3) (n-1))),
    translate ( k/12) (-sqrt 3 * k/12) (rotate  60 (kochLine (k/3) (n-1)))
    ]

-----------------------------------------------------------------------

animation :: Float -> Picture
animation time
	= Scale 0.8 0.8 $ Translate 0 (-300)
	$ tree 4 time (dim $ dim brown)

stump :: Color -> Picture
stump color
	= Color color
	$ Polygon [(30,0), (15,300), (-15,300), (-30,0)]

tree 	:: Int
	-> Float
	-> Color
	-> Picture

tree 0 time color = stump color
tree n time color
 = let	smallTree
		= Rotate (sin time)
		$ Scale 0.5 0.5
		$ tree (n-1) (- time) (greener color)
   in	Pictures
		[ stump color
		, Translate 0 300 $ smallTree
		, Translate 0 240 $ Rotate 20	 smallTree
		, Translate 0 180 $ Rotate (-20) smallTree
		, Translate 0 120 $ Rotate 40 	 smallTree
		, Translate 0  60 $ Rotate (-40) smallTree ]

brown :: Color
brown =  makeColor8 139 100 35  255

greener :: Color -> Color
greener c = mixColors 1 10 green c

-----------------------------------------------------------------------

simulation = Simulation initial step draw

data Ball = Ball Float Float

initial = Ball 100 0
step _ t (Ball x v) = Ball (x + v*t) (v - x*t)
draw (Ball x v) = translate x 0 (circle 20)

