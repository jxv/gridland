module Main where

import Control.Monad (unless)
import GridLand

main :: IO ()
main = runGridLand cfg start update end

cfg :: Config
cfg = Config { cfgRows = 20, cfgCols = 32, cfgTileSize = 32 }

data App = App {
    sprite :: Sprite,
    angle :: Angle,
    location :: Location
}

start :: GridLand () App
start = do
    bkgImg <- loadBackdropImage "data/image.bmp"
    backdrop bkgImg
    sprite <- loadSprite "data/boulder.png"
    music <- loadMusic "data/amen_break.wav"
    playMusic music Nothing
    return App{ sprite = sprite, angle = Degrees 0, location = Location 0 0}

update :: GridLand App Bool
update = do
    app <- getData
    mpos <- getMousePosition
    drawSpriteFront (sprite app) (Tint Red) mpos (angle app)
    let (Degrees d) = angle app
    inputs <- getInputs
    let loc = move (location app) inputs
    print' mpos
    putData app{ angle = Degrees (d + 1), location = mpos }
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
