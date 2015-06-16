module GridLand
    ( test
    ) where

-- SDL
import qualified Graphics.UI.SDL as SDL 
-- SDL-image
import qualified Graphics.UI.SDL.Image as SDL
-- SDL-ttf
import qualified Graphics.UI.SDL.TTF as SDL
-- SDL-mixer
import qualified Graphics.UI.SDL.Mixer as SDL
-- SDL-gfx
import qualified Graphics.UI.SDL.Framerate as SDL
import qualified Graphics.UI.SDL.Primitives as SDL
import qualified Graphics.UI.SDL.Rotozoomer as SDL
-- containers
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
-- grid
import qualified Math.Geometry.Grid as Grid
-- random
import System.Random
-- tuple
import Data.Tuple.Curry
-- htiled
-- import Data.Tiled
-- astar
import Data.Graph.AStar

import System.Exit

width = 640
height = 480

test :: IO ()
test = SDL.withInit [SDL.InitVideo] $ do
    screen <- SDL.setVideoMode 640 480 16 [SDL.SWSurface]
    SDL.setCaption "Test" ""
    SDL.enableUnicode True
    image <- SDL.loadBMP "image.bmp"
    display image
    loop (display image)

display :: SDL.Surface -> IO ()
display image = do
    screen <- SDL.getVideoSurface
    let format = SDL.surfaceGetPixelFormat screen
    red <- SDL.mapRGB format 0xFF 0 0
    green <- SDL.mapRGB format 0 0xFF 0
    SDL.fillRect screen Nothing green
    SDL.fillRect screen (Just (SDL.Rect 10 10 10 10)) red
    posX <- randomRIO (0,width -1 - SDL.surfaceGetWidth image)
    posY <- randomRIO (0,height -1 - SDL.surfaceGetHeight image)
    SDL.blitSurface image Nothing screen (Just (SDL.Rect posX posY 0 0))
    SDL.flip screen


loop :: IO () -> IO ()
loop display = do
    event <- SDL.waitEvent
    case event of
        SDL.Quit -> exitWith ExitSuccess
        SDL.KeyDown (SDL.Keysym _ _ 'q') -> exitWith ExitSuccess
        SDL.KeyDown (SDL.Keysym _ _ ' ') -> display
        _ -> return ()
    loop display
