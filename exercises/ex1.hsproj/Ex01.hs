module Ex01 where

  -- needed to display the picture in the playground
import Codec.Picture

  -- our line graphics programming interface
import LineGraphics


-- Part 1

house :: Path
house = [(300, 750), (300, 450), (270, 450), (500, 200),
          (615, 325), (615, 250), (650, 250), (650, 363),
         (730, 450), (700, 450), (700, 750)]

door :: Path
door = [(420, 750), (420, 550), (580, 550), (580, 750)]

smoke :: Path
smoke = [(635, 240), (625, 230), (635, 220), (625, 210)]

grey :: Colour
grey = (255, 255, 255, 128)

scene :: Picture
scene = [(lightgreen, house), (red, door), (grey, smoke)]


-- Part 2

mid :: Line -> Point
mid ((x1, y1), (x2, y2)) = (x1 + ((x2 - x1) / 2.0), y1 + ((y2 - y1) / 2.0))

tPoint :: Float -> Float -> Point -> Point
tPoint dx dy (x, y) = (x + dx, y + dy)

tLine :: Float -> Float -> Line -> Line
tLine dx dy (p1, p2) = (tPoint dx dy p1, tPoint dx dy p2)

sPoint :: Float -> Point -> Point
sPoint factor (x, y) = (x * factor, y * factor)

scaleLine :: Float -> Line -> Line
scaleLine factor line@(p1@(x1, y1), p2@(x2, y2)) = scaled
  where 
    scaleP = sPoint factor
    (mx, my) = mid line
    (zp1, zp2) = tLine (mx * (-1.0)) (my  * (-1.0)) line
    scaled = tLine mx my (scaleP zp1, scaleP zp2)

line2Box :: Line -> Path
line2Box ((x1, y1), (x2, y2)) = [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]

box2Pic :: Colour -> Path -> Picture
box2Pic colour box = [(colour, box)]

nestedSquares :: Float      -- scale factor
              -> Int        -- number of squares
              -> Colour     -- line colour
              -> Line       -- initial diagonal
              -> Picture             
nestedSquares scale num col ln | num > 0 = (box2Pic col (line2Box ln)) ++ nestedSquares scale (num - 1) col scaledLine
  where scaledLine = scaleLine scale ln
nestedSquares _ _ _ _ = []   
