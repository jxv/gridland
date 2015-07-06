module Main where

import Control.Monad (unless)
import GridLand

main :: IO ()
main = runGridLand cfg start update end

cfg :: Config
cfg = Config { cfgRows = 10, cfgCols = 12, cfgTileSize = 80 }

data App = App {
    fluffy :: Sprite,
    cocoa :: Sprite,
    angle :: Angle,
    location :: Location
}

start :: GridLand () App
start = do
    titleBar "The Adventures of Cocoa & Fluffy"
    bkgImg <- loadBackdropImage "data/bg.jpg"
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

update :: GridLand App Bool
update = do
    app <- getData
    mpos <- getMousePosition
    drawSpriteFront (cocoa app) (Tint LightBlue) mpos (angle app)
    drawSpriteBack (fluffy app) (Tint Yellow) (location app) (angle app)
    let (Degrees d) = angle app
    inputs <- getInputs
    let loc = move (location app) inputs
    putData app{ angle = Degrees (d + 1), location = loc }
    return True

move :: Location -> [Input] -> Location
move loc@Location{..} inputs
    | elem (Key UpArrow Pressed) inputs = Location locX (locY - 1)
    | elem (Key DownArrow Pressed) inputs = Location locX (locY + 1)
    | elem (Key LeftArrow Pressed) inputs = Location (locX - 1) locY
    | elem (Key RightArrow Pressed) inputs = Location (locX + 1) locY
    | otherwise = loc

end :: GridLand App ()
end = return ()
