{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe                      #-}

module GlossAdapters where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import Graphics.Gloss.Interface.Game


data Simulation = forall a. Simulation
    a
    (Float -> a -> a)
    (a -> Picture)


advanceSimulation :: Float -> Simulation -> Simulation
advanceSimulation dt (Simulation w s d) = Simulation (s dt w) s d


simulationToPicture :: Simulation -> Picture
simulationToPicture (Simulation w s d) = d w


data Game = forall a. Game
    a
    (Event -> a -> a)
    (Float -> a -> a)
    (a -> Picture)


advanceGame :: Float -> Game -> Game
advanceGame dt (Game w e s d) = Game (s dt w) e s d


signalGame :: Event -> Game -> Game
signalGame ev (Game w e s d) = Game (e ev w) e s d


gameToPicture :: Game -> Picture
gameToPicture (Game w e s d) = d w
