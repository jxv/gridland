module Main where

import GridLand

main :: IO ()
main = runGridLand (myCfg, myStart, myUpdate, myEnd)

myCfg :: Config
myCfg = mkConfig(10, 10, 64)

myStart :: GridLand () (Sprite, Int)
myStart = do
    bkgImg <- loadBackdropImageStretch("data/image.bmp", Pixelated)
    backdrop bkgImg
    backdrop $ BkdColor(Pink)
    colorSprite <- loadSprite("data/image.bmp")
    return(colorSprite, 0)

myUpdate :: GridLand (Sprite, Int) Bool
myUpdate = do
    (colorSprite, n) <- getData
    drawSpriteFront(colorSprite, Tint Blue, mkLocation(1,2), Degrees n)
    drawSpriteBack(colorSprite, NoFilter, mkLocation(2,2), Degrees (n * 3))
    putData (colorSprite, n + 1)
    return(True)

myEnd :: GridLand (Sprite, Int) ()
myEnd = return ()
