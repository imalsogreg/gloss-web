{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe  #-}

module GlossAdapters where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Interface.Pure.Game
import System.Random

data World where
    WPicture    :: Picture -> World
    WAnimation  :: Float -> (Float -> Picture) -> World
    WSimulation :: a
                -> (Float -> a -> a)
                -> (a -> Picture)
                -> World
    WGame       :: a
                -> (Float -> a -> a)
                -> (Event -> a -> a)
                -> (a -> Picture)
                -> World


data WorldType = PictureType | AnimationType | SimulationType | GameType
    deriving (Eq, Ord)


typeOfWorld :: World -> WorldType
typeOfWorld (WPicture    _      ) = PictureType
typeOfWorld (WAnimation  _ _    ) = AnimationType
typeOfWorld (WSimulation _ _ _  ) = SimulationType
typeOfWorld (WGame       _ _ _ _) = GameType


drawWorld :: World -> Picture
drawWorld (WPicture    p      ) = p
drawWorld (WAnimation  t f    ) = f t
drawWorld (WSimulation w s d  ) = d w
drawWorld (WGame       w s e d) = d w


stepWorld :: Float -> World -> World
stepWorld dt (WPicture    p      ) = WPicture    p
stepWorld dt (WAnimation  t f    ) = WAnimation  (t + dt) f
stepWorld dt (WSimulation w s d  ) = WSimulation (s dt w) s d
stepWorld dt (WGame       w s e d) = WGame       (s dt w) s e d


signalWorld :: Event -> World -> World
signalWorld ev (WPicture    p      ) = WPicture    p
signalWorld ev (WAnimation  t f    ) = WAnimation  t f
signalWorld ev (WSimulation w s d  ) = WSimulation w s d
signalWorld ev (WGame       w s e d) = WGame       (e ev w) s e d


type CompiledWorld = StdGen -> World


worldFromPicture :: Picture -> CompiledWorld
worldFromPicture = const . WPicture


worldFromAnimation :: (Float -> Picture) -> CompiledWorld
worldFromAnimation = const . WAnimation 0


worldFromSimulation :: (StdGen -> a)
                    -> (Float -> a -> a)
                    -> (a -> Picture)
                    -> CompiledWorld
worldFromSimulation i s d g = WSimulation (i g) s d


worldFromGame :: (StdGen -> a)
              -> (Float -> a -> a)
              -> (Event -> a -> a)
              -> (a -> Picture)
              -> CompiledWorld
worldFromGame i s e d g = WGame (i g) s e d
