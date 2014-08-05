module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

leftX :: Float
leftX = -200

rightX :: Float
rightX = 200

width :: Float
width = rightX - leftX

skyColor :: Color
skyColor = makeColor 0.5 0.7 1.0 1.0

main :: IO ()
main = play (InWindow "Flappy λ" (round width, 400) (10, 10)) 
       skyColor 100 startState draw onEvent tick

type Vec2 = (Float,Float)

data Bird = Bird {
    _birdPos :: Vec2,
    _ySpeed :: Float
} deriving (Show)

data Obstacle = Obstacle {
    _obstaclePos :: Vec2,
    _size :: Vec2
}

data GameState = Playing | Dead

data World = World Bird [Obstacle] GameState

makeBird :: Bird
makeBird = Bird (-100, 50) 0

makeObstacles :: [Obstacle]
makeObstacles = [Obstacle (250,100) (50,200),
                 Obstacle (450,-100) (50,200)]

startState :: World
startState = World makeBird makeObstacles Playing

lambda :: Float -> Path
lambda ySpeed = [(-6 - expand, -10), (2 - expand, 8), (4, 8), (1, 0.5), (6 + expand, -10), (3 + expand, -10), (0, -2), (-3, -10)]
    where expand = 1.0 - ySpeed

drawBird :: Bird -> Picture
drawBird (Bird pos ySpeed) = drawAtPos pos $ scale (-2.0) 2.0 $ lineLoop (lambda ySpeed)

drawObstacle :: Obstacle -> Picture
drawObstacle (Obstacle (x, y) (w, h)) = drawAtPos (x, y) $ polygon $ rectanglePath w h

stateColor :: GameState -> Color
stateColor Dead = red
stateColor _ = black

draw :: World -> Picture
draw (World bird obstacles state) = color (stateColor state) $ pictures $ drawBird bird : map drawObstacle obstacles

drawAtPos :: (Float,Float) -> Picture -> Picture
drawAtPos (x, y) = Translate x y

onEvent :: Event -> World -> World
onEvent event (World bird obstacles state) = 
    case event of
        EventKey (SpecialKey KeySpace) Down _ _ -> World (flap bird) obstacles state
        EventKey (Char 'r') Down _ _ -> startState
        _ -> World bird obstacles state

flap :: Bird -> Bird
flap (Bird pos ySpeed) = Bird pos (ySpeed + 2)

updateBird :: Bird -> Bird
updateBird (Bird birdPos ySpeed) = Bird (move (0, ySpeed) birdPos) (ySpeed - 0.03)

moveObstacleLeft :: Obstacle -> Obstacle
moveObstacleLeft (Obstacle pos size) = Obstacle (move (-1.5, 0) pos) size

wrapObstacle :: Obstacle -> Obstacle
wrapObstacle (Obstacle (x, y) size) = Obstacle (if x < (leftX - 50) then rightX + 50 else x, y) size

updateObstacle :: Obstacle -> Obstacle
updateObstacle = wrapObstacle . moveObstacleLeft

updateAll :: World -> World
updateAll w@(World _ _ Dead) = w
updateAll (World bird obstacles state) = World (updateBird bird) (map updateObstacle obstacles) state

detectBorderCollision :: World -> World
detectBorderCollision (World (Bird (x,y) ySpeed) obstacles state) =
    World (Bird (x,y) ySpeed) obstacles (if y < -200 || y > 200 then Dead else state)

isPointInside :: (Float, Float) -> Obstacle -> Bool
isPointInside (x, y) (Obstacle (ox, oy) (w, h)) = and [x > ox - hw, x < ox + hw, y > oy - hh, y < oy + hh]
    where hw = w / 2
          hh = h / 2

detectObstacleCollision :: World -> World
detectObstacleCollision (World (Bird (x,y) ySpeed) obstacles state) =
    World (Bird (x,y) ySpeed) obstacles dead
        where dead = if any (isPointInside (x, y)) obstacles then Dead else state

detectDeath :: World -> World
detectDeath = detectBorderCollision . detectObstacleCollision

tick :: Float -> World -> World
tick _ = detectDeath . updateAll

move :: Num a => (a,a) -> (a,a) -> (a,a)
move (xSpeed, ySpeed) (x, y) = (x + xSpeed, y + ySpeed)
