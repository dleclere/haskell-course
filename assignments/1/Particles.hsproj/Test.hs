module Test where

-- SUBMIT ME

import Test.QuickCheck
import Simulation
import Physics
import TestSupport
import World

prop_EnergyConservation world = 
  realToFrac (abs change) < World.epsilon
    where 
      old = worldEnergy world
      new = worldEnergy $ advanceWorld undefined 0.001 world
      change
        | old == 0 = new
        | otherwise = (new - old) / old