module GridLand
    ( Config(..)
    , Color(..)
    , Stretch(..)
    , ColorFilter(..)
    , Angle(..)
    , Backdrop(..)
    , BackdropImage
    , Sprite
    , Sfx
    , Music
    , GridLand
    , loadSprite
    , loadSpriteStretch
    , loadBackdropImage
    , loadBackdropImageStretch
    , drawSpriteFront
    , drawSpriteMiddle
    , drawSpriteBack
    , print'
    , putStrLn'
    , runGridLand
    , backdrop
    , mkConfig
    , mkLocation
    , getData
    , putData
    , modifyData
    , io
    ) where

import Debug.Trace

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
-- mtl
import qualified Control.Monad.State as State
import qualified Control.Monad.RWS as RWS

import Data.IORef

data Config = Config {
    cfgRows :: Int,
    cfgCols :: Int,
    cfgTileSize :: Int
}

data Foundation = Foundation {
    screen :: SDL.Surface,
    colorMap :: Color -> SDL.Pixel,
    rows :: Int,
    cols :: Int,
    tileSize :: Int
}

data Backdrop
    = BkdColor Color
    | BkdImage BackdropImage

newtype BackdropImage = BackdropImage { bkdImageKey :: Int }
    deriving (Eq, Num, Ord, Show)

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

newtype Sprite = Sprite { spriteKey :: Int } -- (1 + Tinted-N-Replace * Colors) * Rotations
    deriving (Eq, Num, Ord, Show)

data ColorFilter
    = NoFilter
    | Tint Color
    | Replace Color
    deriving (Eq, Show)

data Stretch
    = Smooth
    | Pixelated
    deriving (Eq, Enum, Bounded, Show)

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

newtype Sfx = Sfx { sfxKey :: Int }
    deriving (Eq, Num, Ord, Show)

newtype Music = Music { musicKey :: Int }
    deriving (Eq, Num, Ord, Show)

data Common = Common {
    bkdImages :: Map BackdropImage SDL.Surface,
    sprites :: Map Sprite (V.Vector SDL.Surface),
    sfxs :: Map Sfx Mixer.Chunk,
    musics :: Map Music Mixer.Music,
    currBkd :: Backdrop,
    musicPlaying :: Maybe Music
}

data Todo = Todo {
    todoFrontSprites :: Map (Int, Int) Gfx,
    todoMiddleSprites :: Map (Int, Int) Gfx,
    todoBackSprites :: Map (Int, Int) Gfx,
    todoBackdrop :: Backdrop
}

instance Monoid Backdrop where
    mempty = BkdColor White
    mappend _ a = a

instance Monoid Todo where
    mempty = Todo mempty mempty mempty mempty
    mappend a b = Todo { 
            todoFrontSprites = mappend (todoFrontSprites a) (todoFrontSprites b),
            todoMiddleSprites = mappend (todoMiddleSprites a) (todoMiddleSprites b),
            todoBackSprites = mappend (todoBackSprites a) (todoBackSprites b),
            todoBackdrop = mappend (todoBackdrop a) (todoBackdrop b)
        }

newtype GridLand a b = GridLand { unGridLand :: RWS.RWST Foundation Todo(Common, a) IO b }
    deriving
        (Functor, Applicative, Monad, MonadIO, RWS.MonadRWS Foundation Todo (Common, a)
        ,MonadReader Foundation, MonadWriter Todo, MonadState (Common, a))
 
mkConfig :: (Int, Int, Int) -> Config
mkConfig = uncurryN Config

mkLocation :: (Int, Int) -> Location
mkLocation (x,y) = Location { locX = x, locY = y }

mkTodo :: Todo
mkTodo = Todo Map.empty Map.empty Map.empty (BkdColor White)

getData :: GridLand a a
getData = RWS.gets snd

putData :: a -> GridLand a ()
putData a = RWS.modify . second $ const a

modifyData :: (a -> a) -> GridLand a ()
modifyData f = RWS.modify . second $ f

spriteDegrees :: [Int]
spriteDegrees = [ rot * 360 `div` rotations | rot <- [0 .. (rotations - 1)]]

spriteOpts :: [(ColorFilter, Int)]
spriteOpts = noFilters ++ filters
 where
    noFilters = [(NoFilter, theta) | theta <- spriteDegrees]
    filters = [(cf c, theta) | cf <- [Tint, Replace], c <- [minBound..maxBound], theta <-  spriteDegrees]

loadSpriteStretch :: (FilePath, Stretch) -> GridLand a Sprite
loadSpriteStretch (path, stretch) = do
    base <- liftIO $ Image.load path
    let (w,h) = (SDL.surfaceGetWidth base, SDL.surfaceGetHeight base)
    let side = max w h 
    ts <- RWS.asks tileSize
    let zoom = (fromIntegral ts) / (fromIntegral side)
    let drw (_, theta) = do
            Gfx.rotozoom base (fromIntegral theta) zoom (stretch == Smooth)
    frames <- liftIO $ mapM drw spriteOpts
    liftIO $ SDL.freeSurface base
    let spriteFrames = V.fromListN totalFrames frames
    key <- RWS.gets (Map.size . sprites . fst)
    let spr = Sprite key
    RWS.modify . first $ \s -> s { sprites = Map.insert spr spriteFrames (sprites s) }
    return spr

loadSprite :: (FilePath) -> GridLand a Sprite
loadSprite (path) = loadSpriteStretch (path, Smooth)

copySurface :: SDL.Surface -> IO SDL.Surface
copySurface = SDL.displayFormat

loadBackdropImageStretch :: (FilePath, Stretch) -> GridLand a Backdrop
loadBackdropImageStretch (path, stretch) = do
    base <- liftIO $ Image.load path
    scr <- RWS.asks screen
    let (scrW,scrH) = (SDL.surfaceGetWidth scr, SDL.surfaceGetHeight scr)
    let (w,h) = (SDL.surfaceGetWidth base, SDL.surfaceGetHeight base)
    let (zoomW, zoomH) = (fromIntegral scrW / fromIntegral w, fromIntegral scrH / fromIntegral h)
    zoomed <- liftIO $ Gfx.zoom base zoomW zoomH (stretch == Smooth)
    key <- RWS.gets (Map.size . bkdImages . fst)
    let bi = BackdropImage key
    RWS.modify . first $ \s -> s { bkdImages = Map.insert bi zoomed (bkdImages s) }
    liftIO $ SDL.freeSurface base
    return (BkdImage bi)

loadBackdropImage :: (FilePath) -> GridLand a Backdrop
loadBackdropImage (path) = loadBackdropImageStretch (path, Smooth)

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
rotations = 8 -- 36 
colors = 1 + fromEnum (maxBound :: Color)
withColors = 2
withoutColors = 1
totalFrames = (withoutColors + withColors * colors) * rotations
colorAngleInterval = colors * rotations

colorAngleOffset :: Color -> Angle -> Int
colorAngleOffset c t = colorOffset c + angleOffset t

angleOffset :: Angle -> Int
angleOffset (Radians r) = angleOffset $ Degrees (round $ 180 * r / pi)
angleOffset (Degrees d) = ((d `mod` 360) * rotations `div` 360)

colorOffset :: Color -> Int
colorOffset c = fromEnum c * rotations

frameOffset :: ColorFilter -> Angle -> Int
frameOffset NoFilter theta = angleOffset theta
frameOffset (Tint c) theta = rotations + colorAngleOffset c theta
frameOffset (Replace c) theta = rotations + colorAngleInterval + colorAngleOffset c theta

spriteGfx :: Sprite -> ColorFilter -> Location -> Angle -> GridLand a Gfx
spriteGfx spr cf loc theta = do
    let offset = frameOffset cf theta
    sprMap <- RWS.gets (sprites . fst)
    let sur = (sprMap Map.! spr) V.! offset
    return $ Gfx sur loc

gfxRect :: Int -> Location -> SDL.Surface -> SDL.Rect
gfxRect ts Location{..} sur = let
    (w, h) = (SDL.surfaceGetWidth sur, SDL.surfaceGetHeight sur)
    in SDL.Rect (ts * locX) (ts * locY) ts ts

drawSpriteFront :: (Sprite, ColorFilter, Location, Angle)  -> GridLand a ()
drawSpriteFront (sprite, cf, loc, theta) = do
    gfx <- spriteGfx sprite cf loc theta
    RWS.tell $ mempty { todoFrontSprites = Map.singleton (locX loc, locY loc) gfx }

drawSpriteMiddle :: (Sprite, ColorFilter, Location, Angle)  -> GridLand a ()
drawSpriteMiddle (sprite, cf, loc, theta) = do
    gfx <- spriteGfx sprite cf loc theta
    RWS.tell $ mempty { todoMiddleSprites = Map.singleton (locX loc, locY loc) gfx }

drawSpriteBack :: (Sprite, ColorFilter, Location, Angle)  -> GridLand a ()
drawSpriteBack (sprite, cf, loc, theta) = do
    gfx <- spriteGfx sprite cf loc theta
    RWS.tell $ mempty { todoBackSprites = Map.singleton (locX loc, locY loc) gfx }

drawSprite :: (Sprite, ColorFilter, Location, Angle)  -> GridLand a ()
drawSprite (sprite, cf, loc, theta) = do
    (s, ts) <- RWS.asks (screen &&& tileSize)
    Gfx{..} <- spriteGfx sprite cf loc theta
    let (w, h) = (SDL.surfaceGetWidth gfxSurface, SDL.surfaceGetHeight gfxSurface)
    let tileRect = Just $ SDL.Rect ((w - ts) `div` 2) ((h - ts) `div` 2) ts ts
    void . liftIO $ SDL.blitSurface gfxSurface tileRect s (Just $ gfxRect ts gfxLocation gfxSurface)
 
drawGfx :: SDL.Surface -> Int -> Gfx -> IO ()
drawGfx scr ts Gfx{..} = do
    let (w, h) = (SDL.surfaceGetWidth gfxSurface, SDL.surfaceGetHeight gfxSurface)
    let tileRect = Just $ SDL.Rect ((w - ts) `div` 2) ((h - ts) `div` 2) ts ts
    void $ SDL.blitSurface gfxSurface tileRect scr (Just $ gfxRect ts gfxLocation gfxSurface)

stopMusic :: Music -> GridLand a ()
stopMusic Music{..} = do
    mplaying <- RWS.gets (musicPlaying . fst)
    case mplaying of
        Nothing -> return ()
        Just (Music key) -> when (musicKey == key) $ liftIO Mixer.haltMusic
    
stopAllMusic :: GridLand a ()
stopAllMusic = liftIO Mixer.haltMusic

-- | (Config, Start, Update, End) -> IO ()
runGridLand :: (Config, GridLand () a, GridLand a Bool, GridLand a ()) -> IO ()
runGridLand (cfg, onStart, onUpdate, onEnd) = do
    foundation <- start cfg
    let common = mkCommon
    (initUserData, (initCommon,_), _) <- RWS.runRWST (unGridLand onStart) foundation (common, ())
    let initState = (initCommon, initUserData)
    let update = onUpdate >> drawBackdrop
    let dgfx = drawGfx (screen foundation) (tileSize foundation)
    endState <- ($ initState) $ fix $ \loop state -> do
        startTick <- SDL.getTicks
        inputs <- getInputs
        (continue, state', todo) <- RWS.runRWST (unGridLand update) foundation state
        mapM_ dgfx (Map.elems $ todoBackSprites todo)
        mapM_ dgfx (Map.elems $ todoMiddleSprites todo)
        mapM_ dgfx (Map.elems $ todoFrontSprites todo)
        liftIO $ SDL.flip (screen foundation)
        endTick <- SDL.getTicks
        let diff = endTick - startTick
        when (diff < 16) (SDL.delay $ 16 - diff)
        if elem Quit inputs
            then return state'
            else loop state'
    void $ RWS.execRWST (unGridLand onEnd) foundation endState
    end foundation common

start :: Config -> IO Foundation
start Config{..} = do
    SDL.init [SDL.InitEverything]
    screen <- SDL.setVideoMode (cfgCols * cfgTileSize) (cfgRows * cfgTileSize) 16 [SDL.SWSurface]
    SDL.setCaption "Grid Land" ""
    SDL.enableUnicode True
    colorMap <- getColorMap screen
    return $ Foundation screen colorMap cfgRows cfgCols cfgTileSize

getColorMap :: SDL.Surface -> IO (Color -> SDL.Pixel)
getColorMap s = do
    let fmt = SDL.surfaceGetPixelFormat s
    pixels <- forM [minBound..maxBound] $ \c -> liftIO $ colorValue' c (SDL.mapRGB fmt)
    let table = V.fromList pixels
    return $ \c -> table V.! (fromEnum c)

mkCommon :: Common
mkCommon = Common Map.empty Map.empty Map.empty Map.empty (BkdColor White) Nothing

end :: Foundation -> Common -> IO ()
end Foundation{..} Common{..} = do
    mapM_ SDL.freeSurface (Map.elems bkdImages)
    mapM_ (mapM_ SDL.freeSurface . V.toList) (Map.elems sprites)
    mapM_ Mixer.freeMusic (Map.elems musics)
    mapM_ (const $ return ()) (Map.elems sfxs) -- Mixer.freeChunks is missing a binding
    SDL.freeSurface screen
    SDL.quit

putStrLn' :: String -> GridLand a ()
putStrLn' = liftIO . putStrLn

print' :: Show b => b -> GridLand a ()
print' = liftIO . print 

titleBar :: String -> GridLand a ()
titleBar title = liftIO $ SDL.setCaption title ""

backdrop :: Backdrop -> GridLand a ()
backdrop b = RWS.modify . first $ \s -> s { currBkd = b }

drawBackdrop :: GridLand a ()
drawBackdrop = do
    (s, cmap) <- RWS.asks (screen &&& colorMap)
    b <- RWS.gets (currBkd . fst)
    case b of
        BkdColor c -> void . liftIO $ SDL.fillRect s Nothing (cmap c)
        BkdImage bi -> do
            img <- RWS.gets ((Map.! bi) . bkdImages . fst)
            void . liftIO $ SDL.blitSurface img Nothing s Nothing

io :: IO b -> GridLand a b
io = liftIO
