module Main where

import Control.Monad (unless)
import GridLand

main :: IO ()
main = runGridLand (cfg, start, update, end)

cfg :: Config
cfg = Config { cfgRows = 20, cfgCols = 30, cfgTileSize = 32 }

data App = App {
    colorSprite :: Sprite,
    degrees :: Int
}

start :: GridLand () App
start = do
    bkgImg <- loadBackdropImage "data/image.bmp"
    backdrop bkgImg
    backdrop (BkdColor Red)
    colorSprite <- loadSprite "data/image.bmp"
    music <- loadMusic "data/amen_break.wav"
    playMusic music Nothing
    return (App colorSprite 0)

update :: GridLand App Bool
update = do
    app <- getData
    drawSpriteFront (colorSprite app) (Tint Blue) (Location 1 2) (Degrees (degrees app))
    drawSpriteFront (colorSprite app) (Tint Blue) (Location 2 2) (Degrees (degrees app))
    drawSpriteFront (colorSprite app) (Replace Blue) (Location 2 3) (Degrees (degrees app))
    drawSpriteFront (colorSprite app) (Tint Blue) (Location 3 2) (Degrees (degrees app))
    drawSpriteBack (colorSprite app) (Tint Orange) (Location 3 3) (Degrees (3 * degrees app))
    putData app{ degrees = degrees app + 1 }
    return True

end :: GridLand App ()
end = return ()
