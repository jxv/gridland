module Main where

import Control.Monad (unless)
import GridLand

main :: IO ()
main = runGridLand cfg start update (return ())

cfg :: Config
cfg = Config { cfgRows = 9, cfgCols = 12, cfgTileSize = 80 }

-- App :: Sprite -> Sprite -> Angle -> Location -> App
data App = App {
    fluffy :: Sprite,
    cocoa :: Sprite,
    angle :: Angle, -- Degrees, Radians,
    cocoaLocations :: [Location],
    fluffyLocations :: [Location]
}

start :: GridLand () App
start = do
    titleBar "The Adventures of Cocoa & Fluffy"
    bkgImg <- loadBackdropImage "data/image.bmp"
    backdrop bkgImg
    cocoaSprite <- loadSprite "data/cocoa.png"
    fluffySprite <- loadSprite "data/lion.png"
    music <- loadMusic "data/amen_break.wav"
    playMusic music Nothing
    return App {
            fluffy = fluffySprite,
            cocoa = cocoaSprite,
            angle = Degrees 0,
            cocoaLocations = replicate 20 (Location 0 0),
            fluffyLocations = replicate 50 (Location 0 0)
        }

update :: GridLand App ()
update = do
    updateAngle
    updateCocoas
    updateFluffies

updateAngle :: GridLand App ()
updateAngle = do
    app <- getData
    let (Degrees d) = angle app
    putData app { angle = Degrees (d + 1) }

updateFluffies :: GridLand App ()
updateFluffies = do
    app <- getData
    inputs <- getInputs
    let loc = move (head $ fluffyLocations app) inputs
    let locs = if loc == (head $ fluffyLocations app)
            then fluffyLocations app
            else loc : (init $ fluffyLocations app)
    let draw (loc,color) = drawSpriteMiddle (fluffy app) (Tint color) loc (angle app)
    mapM_ draw (zip locs $ cycle [minBound .. maxBound])
    putData app { fluffyLocations = locs }

updateCocoas :: GridLand App ()
updateCocoas = do
    app <- getData
    mpos <- getMousePosition
    let locs = if mpos == (head $ cocoaLocations app) -- Update if there's change in mouse position
            then cocoaLocations app
            else mpos : (init $ cocoaLocations app)
    let draw (loc,color) = drawSpriteFront (cocoa app) (Tint color) loc (angle app)
    mapM_ draw (zip locs $ cycle [minBound .. maxBound])
    putData app { cocoaLocations = locs }

move :: Location -> [Input] -> Location
move loc inputs =
    if elem (Key UpArrow Pressed) inputs
    then Location (locX loc) (locY loc - 1)
    else if elem (Key DownArrow Pressed) inputs
    then Location (locX loc) (locY loc + 1)
    else if elem (Key LeftArrow Pressed) inputs
    then Location (locX loc - 1) (locY loc)
    else if elem (Key RightArrow Pressed) inputs
    then Location (locX loc + 1) (locY loc)
    else loc

