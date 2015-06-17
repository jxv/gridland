module GridLand
    ( test
    ) where

import Import
-- SDL
import qualified Graphics.UI.SDL as SDL 
import qualified Graphics.UI.SDL.Video as SDL 
-- SDL-image
import qualified Graphics.UI.SDL.Image as Image
-- SDL-ttf
import qualified Graphics.UI.SDL.TTF as TTF
-- SDL-mixer
import qualified Graphics.UI.SDL.Mixer as Mixer
-- SDL-gfx
import qualified Graphics.UI.SDL.Framerate as Gfx
import qualified Graphics.UI.SDL.Primitives as Gfx
import qualified Graphics.UI.SDL.Rotozoomer as Gfx
-- containers
import qualified Data.Map as Map
import qualified Data.Set as Set
-- grid
import qualified Math.Geometry.Grid as Grid
-- vector
import qualified Data.Vector as V

import Data.IORef

data Color
    = Red
    | Orange
    | Yellow
    | YellowGreen
    | Green
    | LightBlue
    | Blue
    | Pink
    | Black
    | White
    deriving (Enum, Eq, Bounded, Show)

data Input
    = Quit
    | Input
    deriving (Eq, Show)

data Sprite = Sprite {
    spriteFrames :: V.Vector SDL.Surface -- (1 + Tinted-N-Fully * Colors) * Rotations
}

class ToSprite a where
    makeSprite :: (GfxMachine, a) -> Sprite

data ColorFilter
    = NoFilter
    | Tint Color
    | Replace Color
    deriving (Eq, Show)

data Angle
    = Radians Float
    | Degrees Int
    deriving (Eq, Show)

data Location = Location {
    locX :: Int,
    locY :: Int
}

data Gfx = Gfx {
    gfxSurface :: SDL.Surface,
    gfxLocation :: Location
}

type Sfx = Int
type Music = Int

data GfxMachine = GfxMachine {
    gmGridSize :: (Int, Int),
    gmTileSize :: Int,
    gmScreen :: SDL.Surface
}

data SfxMachine = SfxMachine

data MusicMachine = MusicMachine

type MasterMachine = IO

test :: IO ()
test = execute (8, 8, 64, step)

type Step = ([Input], GfxMachine, SfxMachine, MusicMachine) -> MasterMachine Bool

step :: Step
step (inputs, gfxMach, sfxMach, musMach) = do
    draw (gfxMach, [])
    return True

execute :: (Int, Int, Int, Step) -> IO ()
execute (w, h, tileSize, step) = do
    (gfxMach, sfxMach, musMach) <- start (w, h, tileSize)
    loopMachine (gfxMach, sfxMach, musMach, step)
    end (gfxMach, sfxMach, musMach)

pollEvents :: IO [SDL.Event]
pollEvents = do
    event <- SDL.pollEvent
    if event == SDL.NoEvent
        then return []
        else (:) <$> return event <*> pollEvents

getInputs :: IO [Input]
getInputs = do
    events <- pollEvents
    return $ foldr cvt [] events
 where
    cvt SDL.Quit inputs = Quit : inputs
    cvt _ inputs = inputs

loopMachine :: (GfxMachine, SfxMachine, MusicMachine, Step) -> MasterMachine ()
loopMachine args@(gfxMach, sfxMach, musMach, step) = do
    startTick <- SDL.getTicks
    inputs <- getInputs
    continue <- step (inputs, gfxMach, sfxMach, musMach)
    endTick <- SDL.getTicks
    let diff = endTick - startTick
    when (diff < 16) (SDL.delay $ 16 - diff)
    when (continue || elem Quit inputs) (loopMachine args)

colorValue :: Integral a => Color -> (a, a, a)
colorValue = \case
  Red -> (0xcc, 0x00, 0x00)
  Orange -> (0xff, 0xa5, 0x00)
  Yellow -> (0xff, 0xff, 0x00)
  YellowGreen -> (0x7f, 0xff, 0x00)
  Green -> (0x00, 0x66, 0x00)
  LightBlue -> (0x66, 0xff, 0xff)
  Blue -> (0x00, 0x00, 0x99)
  Pink -> (0xff, 0x66, 0x99)
  Black -> (0x00, 0x00, 0x00)
  White -> (0xff, 0xff, 0xff)

colorValue' :: Integral a => Color -> (a -> a -> a -> b) -> b
colorValue' c f = uncurryN f (colorValue c)

rotations, colors, withoutColors, withColors, totalFrames, colorAngleInterval:: Int
rotations = 30
colors = 1 + fromEnum (maxBound :: Color)
withColors = 2
withoutColors = 1
totalFrames = (withoutColors + withColors * colors) * rotations
colorAngleInterval = colors * rotations

angleOffset :: Angle -> Int
angleOffset (Radians r) = angleOffset $ Degrees (round $ 180 * r / pi)
angleOffset (Degrees d) = ((d `mod` 360) * rotations `div` 360)

colorOffset :: Color -> Int
colorOffset c = fromEnum c * rotations

colorAngleOffset :: Color -> Angle -> Int
colorAngleOffset c t = colorOffset c + angleOffset t

frameOffset :: ColorFilter -> Angle -> Int
frameOffset NoFilter theta = angleOffset theta
frameOffset (Tint c) theta = rotations + colorAngleOffset c theta
frameOffset (Replace c) theta = rotations + colorAngleInterval + colorAngleOffset c theta

spriteGfx :: (Sprite, ColorFilter, Location, Angle) -> Gfx
spriteGfx (Sprite{..}, cf, loc, theta) = let
    offset = frameOffset cf theta
    sur = spriteFrames V.! offset
    in Gfx sur loc

gfxRect :: Int -> Location -> SDL.Rect
gfxRect ts Location{..} = SDL.Rect (ts * locX) (ts * locY) (ts * (locX + 1)) (ts * (locY + 1)) 

draw :: (GfxMachine, [Gfx]) -> MasterMachine ()
draw (GfxMachine{..}, gfxs) = do
    let format = SDL.surfaceGetPixelFormat gmScreen
    white <- colorValue' White $ SDL.mapRGB format
    void $ SDL.fillRect gmScreen Nothing white
    let drw Gfx{..} = SDL.blitSurface gfxSurface (Just $ gfxRect gmTileSize gfxLocation) gmScreen Nothing
    mapM_ drw gfxs
    SDL.flip gmScreen

playSfx :: (SfxMachine, [Sfx]) -> MasterMachine ()
playSfx (mach, sfxs) = do
    return ()

playMusic :: (MusicMachine,  [Music]) -> MasterMachine ()
playMusic (mach, musics) = do
    return ()

start :: (Int, Int, Int) -> IO (GfxMachine, SfxMachine, MusicMachine)
start (w, h, tileSize) = do
    SDL.init [SDL.InitEverything]
    screen <- SDL.setVideoMode (w * tileSize) (h * tileSize) 16 [SDL.SWSurface]
    SDL.setCaption "Grid Land" ""
    SDL.enableUnicode True
    let gm = GfxMachine (w,h) tileSize screen
    let sm = SfxMachine
    let mm = MusicMachine
    return (gm, sm, mm)

end :: (GfxMachine, SfxMachine, MusicMachine) -> IO ()
end (GfxMachine{..}, SfxMachine, MusicMachine) = do
    SDL.freeSurface gmScreen
    SDL.quit
