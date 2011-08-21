{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Safe                      #-}

module GlossAdapters where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate

data Simulation = forall a. Simulation
    a
    (ViewPort -> Float -> a -> a)
    (a -> Picture)


advanceSimulation :: Float -> Simulation -> Simulation
advanceSimulation t (Simulation c s p) = Simulation (s v t c) s p
    where v = ViewPort (0,0) 0 1


simulationToPicture :: Simulation -> Picture
simulationToPicture (Simulation c s p) = p c

