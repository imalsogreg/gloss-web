{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe                      #-}

module GlossAdapters where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Graphics.Gloss.Interface.Game


data Simulation = forall a. Simulation
    a
    (ViewPort -> Float -> a -> a)
    (a -> Picture)


advanceSimulation :: Float -> Simulation -> Simulation
advanceSimulation t (Simulation c s p) = Simulation (s v t c) s p
    where v = ViewPort (0,0) 0 1


simulationToPicture :: Simulation -> Picture
simulationToPicture (Simulation c s p) = p c


data Game = forall a. Game
    a
    (Event -> a -> a)
    (Float -> a -> a)
    (a -> Picture)


advanceGame :: Float -> Game -> Game
advanceGame t (Game w e a d) = Game (a t w) e a d


signalGame :: Event -> Game -> Game
signalGame ev (Game w e a d) = Game (e ev w) e a d


gameToPicture :: Game -> Picture
gameToPicture (Game w e a d) = d w

