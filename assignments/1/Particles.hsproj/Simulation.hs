module Simulation (moveParticle, accelerate, advanceWorld) where

-- SUBMIT ME  

--You need to define three functions in this module skeleton

-- DO NOT CHANGE ANY OF THE TYPE SIGNATURES IN THIS FILE

import World
import Physics


--type Point     = (Float, Float)     -- Same definitions as...
--type Vector    = Point              -- in Graphics.Gloss.Picture
--type Mass      = Float              -- in kilogram
--type Position  = Point              -- in meter
--type Velocity  = Vector             -- in meter/second
--type Accel     = Vector             -- in meter/second^2
--type Energy    = Double             -- in joule
-- data Particle = Particle Mass Position Velocity


-- Move a particle according to its velocity for the given number of (simulated) seconds.
-- delta_r = v * delta_t
--

(.++) :: Num a => (a, a) -> (a, a) -> (a, a)
(.++) (ax1, ay1) (ax2, ay2) = (ax1 + ax2, ay1 + ay2)

(.*) :: Num a => (a, a) -> a -> (a, a)
(.*) (x, y) scalar = (x * scalar, y * scalar)

moveParticle :: Float -> Particle -> Particle
moveParticle dt (Particle mass pos v) = 
  Particle mass (pos .++ (v .* dt)) v
    
--force :: Particle -> Particle -> Accel
-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.

calcAccel :: [Particle] -> Particle -> Accel
calcAccel ps p = foldl (.++) (0.0, 0.0) (map (force p) ps)

accelParticle :: Particle -> Float -> Accel -> Particle
accelParticle (Particle mass pos v) dt a =
  Particle mass pos (v .++ (a .* dt))

accelerate :: Float -> [Particle] -> [Particle]
accelerate dt ps = map (accel) ps
  where 
    accel p = accelParticle p dt $ calcAccel ps p

-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ dt (World s1 s2 s3 particles) = 
  World s1 s2 s3 $ map (moveParticle sdt) (accelerate sdt particles)
    where sdt = dt * s3
