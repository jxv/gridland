module Main where

import Control.Monad (unless)
import GridLand

main :: IO ()
main = runGridLand cfg start update (return ())

cfg :: Config
cfg = Config { cfgRows = 9, cfgCols = 12, cfgTileSize = 80 }

data App = App {
    fluffy :: Sprite,
    cocoa :: Sprite,
    angle :: Angle, -- Degrees, Radians
    location :: Location
}

start :: GridLand () App
start = do
    titleBar "The Adventures of Cocoa & Fluffy"
    bkgImg <- loadBackdropImage "data/lion.png"
    backdrop bkgImg
    cocoaSprite <- loadSprite "data/cocoa.png"
    fluffySprite <- loadSprite "data/lion.png"
    music <- loadMusic "data/amen_break.wav"
    playMusic music Nothing
    return App {
            fluffy = fluffySprite,
            cocoa = cocoaSprite,
            angle = Degrees 0,
            location = Location 0 0
        }

update :: GridLand App ()
update = do
    app <- getData
    mpos <- getMousePosition
    drawSpriteFront (cocoa app) (Replace Blue) mpos (angle app)
    drawSpriteMiddle (cocoa app) NoFilter (Location 3 3) (Degrees 0)
    drawSpriteBack (fluffy app) (Replace Yellow) (location app) (angle app)
    let (Degrees d) = angle app
    inputs <- getInputs
    let loc = move (location app) inputs
    putData app{ angle = Degrees (d + 1), location = loc }

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

