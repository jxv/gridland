module Main where

import Control.Monad (unless)
import GridLand

main :: IO ()
main = runGridLand (cfg, start, update, end)

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
    backdrop (BkdColor Red)
    sprite <- loadSprite "data/boulder.png"
    music <- loadMusic "data/amen_break.wav"
    playMusic music Nothing
    return App{ sprite = sprite, angle = Degrees 0, location = Location 0 0}

update :: GridLand App Bool
update = do
    app <- getData
    drawSpriteFront (sprite app) NoFilter (location app) (angle app)
    drawSpriteMiddle (sprite app) (Tint Blue) (Location 2 2) (angle app)
    drawSpriteMiddle (sprite app) (Replace Green) (Location 2 3) (angle app)
    drawSpriteMiddle (sprite app) (Tint Orange) (Location 3 2) (angle app)
    let (Degrees d) = angle app
    inputs <- getInputs
    let loc = location app
    let loc' =
            if elem (Key UpArrow Pressed) inputs
            then Location (locX loc) (locY loc - 1)
            else if elem (Key DownArrow Pressed) inputs
                then Location (locX loc) (locY loc + 1)
                else if elem (Key LeftArrow Pressed) inputs
                    then Location (locX loc - 1) (locY loc)
                    else if elem (Key RightArrow Pressed) inputs
                        then Location (locX loc + 1) (locY loc)
                        else loc
    putData app{ angle = Degrees (d + 1), location = loc' }
    return True

end :: GridLand App ()
end = return ()
