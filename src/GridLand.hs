module GridLand
    ( Config(..)
    , Color(..)
    , Stretch(..)
    , ColorFilter(..)
    , Angle(..)
    , Backdrop(..)
    , Input(..)
    , Key(..)
    , KeyState(..)
    , Location(..)
    , ToSprite(..)
    , BackdropImage
    , Sprite
    , Sfx
    , Music
    , GridLand
    , titleBar
    , loadSprite
    , loadSpriteStretch
    , loadBackdropImage
    , loadBackdropImageStretch
    , drawSpriteFront
    , drawSpriteMiddle
    , drawSpriteBack
    , loadMusic
    , playMusic
    , stopMusic
    , stopAllMusic
    , loadSfx
    , playSfx
    , print'
    , putStrLn'
    , runGridLand
    , backdrop
    , getData
    , putData
    , modifyData
    , getPlayingMusic
    , io
    , getInputs
    , getMousePosition
    ) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Video as SDL
import qualified Graphics.UI.SDL.Image as Image
-- import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL.Mixer as Mixer
import qualified Graphics.UI.SDL.Framerate as Gfx
import qualified Graphics.UI.SDL.Primitives as Gfx
import qualified Graphics.UI.SDL.Rotozoomer as Gfx
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Math.Geometry.Grid as Grid
import qualified Data.Vector as V
import qualified Control.Monad.State as State
import qualified Control.Monad.RWS as RWS
import qualified Data.Array as Array
import qualified Safe as Safe

import Debug.Trace
import Data.IORef
import Data.Char
import Data.Maybe
import Control.Arrow
import System.Environment

import GridLand.Import
import GridLand.Data
import GridLand.SDL
import GridLand.Color

newTodo :: Todo
newTodo = Todo Map.empty Map.empty Map.empty (BkdColor White)

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

loadSpriteStretch :: FilePath -> Stretch -> GridLand a Sprite
loadSpriteStretch rawPath stretch = do
    path <- correctPath rawPath
    base <- liftIO $ Image.load path >>= SDL.displayFormat >>= setColorKey (0xff,0x00,0xff)
    let (w,h) = (SDL.surfaceGetWidth base, SDL.surfaceGetHeight base)
    let side = max w h
    ts <- RWS.asks tileSize
    let zoom = (fromIntegral ts) / (fromIntegral side)
    fmt <- io (surfacePixelFormat base)
    let blitter = blit stretch base zoom
    frames <- liftIO $ mapM (\(cf, theta) -> blitter (mapFilterColor fmt cf) theta) spriteOpts
    liftIO $ SDL.freeSurface base
    let spriteFrames = V.fromListN totalFrames frames
    key <- RWS.gets (Map.size . sprites . fst)
    let spr = Sprite key
    RWS.modify . first $ \s -> s { sprites = Map.insert spr spriteFrames (sprites s) }
    return spr

blit
    :: Stretch
    -> SDL.Surface
    -> Double
    -> (Word32 -> SDL.Pixel)
    -> Int
    -> IO SDL.Surface
blit stretch base zoom toPixel theta = do
    rotozoom <- Gfx.rotozoom base (fromIntegral theta) zoom (stretch == Smooth)
    ping <- SDL.displayFormat rotozoom >>= setColorKey (0xff, 0x00, 0xff)
    pong <- setColorKey (0xff, 0x00, 0xff) rotozoom
    let (w,h) = (SDL.surfaceGetWidth rotozoom, SDL.surfaceGetHeight rotozoom)
    let fmt = SDL.surfaceGetPixelFormat rotozoom
    forM_ ([(x,y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]) $ \(x,y) -> do
        (SDL.Pixel v) <- getPixel32 x y ping
        putPixel32 x y (toPixel v) pong
    SDL.freeSurface ping
    return pong

mapFilterColor :: PixelFormat -> ColorFilter -> Word32 -> SDL.Pixel
mapFilterColor RGBA = filterColorRGBA
mapFilterColor BGRA = filterColorBGRA
mapFilterColor ARGB = filterColorARGB
mapFilterColor ABGR = filterColorABGR

filterColorABGR :: ColorFilter -> Word32 -> SDL.Pixel
filterColorABGR = mkFilterColor toColorABGR fromColorABGR 0xff000000

filterColorARGB :: ColorFilter -> Word32 -> SDL.Pixel
filterColorARGB = mkFilterColor toColorARGB fromColorARGB 0xff000000

filterColorRGBA :: ColorFilter -> Word32 -> SDL.Pixel
filterColorRGBA = mkFilterColor toColorRGBA fromColorRGBA 0x000000ff

filterColorBGRA :: ColorFilter -> Word32 -> SDL.Pixel
filterColorBGRA = mkFilterColor toColorBGRA fromColorBGRA 0x000000ff

mkFilterColor
    :: (Word8 -> Word8 -> Word8 -> Word32)
    -> (Word32 -> (Word8, Word8, Word8))
    -> Word32
    -> ColorFilter
    -> Word32
    -> SDL.Pixel
mkFilterColor toColor fromColor transMask = let
    magenta = toColor 0xff 0x00 0xff
    white = toColor 0xff 0xff 0xff
    trans = xor magenta transMask
    in filterColor toColor fromColor white magenta trans

filterColor
    :: (Word8 -> Word8 -> Word8 -> Word32)
    -> (Word32 -> (Word8, Word8, Word8))
    -> Word32
    -> Word32
    -> Word32
    -> ColorFilter
    -> Word32
    -> SDL.Pixel
filterColor _ _ _ _ _ NoFilter v = SDL.Pixel v
filterColor toColor fromColor white magenta trans (Tint color) v = let
    (cr, cg, cb) = fromColor v
    (tr, tg, tb) = colorValue color
    (r, g, b) = (shiftR tr 1 + shiftR cr 1, shiftR tg 1 + shiftR cg 1, shiftR tb 1 + shiftR cb 1)
    tinted = toColor r g b
    in SDL.Pixel $ if magenta == toColor cr cg cb .&. white
        then trans
        else tinted
filterColor toColor fromColor white magenta trans (Replace color) v = let
    (cr, cg, cb) = fromColor v
    (r, g, b) = colorValue color
    p = toColor r g b
    in SDL.Pixel $ if magenta == (toColor cr cg cb) .&. white
        then trans
        else p

-- This is a necessary hack because the SDL bindings are insufficient
correctPath :: FilePath -> GridLand a FilePath
correctPath "" = RWS.gets (pathPrefix . fst)
correctPath path =
    if head path == '/'
    then return path
    else do
        prefix <- RWS.gets (pathPrefix . fst)
        return $ prefix ++ path

loadSprite :: FilePath -> GridLand a Sprite
loadSprite path = loadSpriteStretch path Pixelated

loadBackdropImageStretch :: FilePath -> Stretch -> GridLand a Backdrop
loadBackdropImageStretch rawPath stretch = do
    path <- correctPath rawPath
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

loadBackdropImage :: FilePath -> GridLand a Backdrop
loadBackdropImage path = loadBackdropImageStretch path Smooth

pollEvents :: IO [SDL.Event]
pollEvents = do
    event <- SDL.pollEvent
    if event == SDL.NoEvent
        then return []
        else (:) <$> return event <*> pollEvents

toMousePosition :: Foundation -> (Word16, Word16) -> Location
toMousePosition Foundation{..} (x,y) = Location {
        locX = fromIntegral x `div` tileSize,
        locY = fromIntegral y `div` tileSize
    }

pollInputs :: Foundation -> IO ([Input], Maybe Location)
pollInputs foundation = do
    events <- pollEvents
    return (foldr cvt [] events, mousePos events)
 where
    mousePos = Safe.headMay . catMaybes . map mpos
    mpos (SDL.MouseMotion x y _ _) = let
        pos = toMousePosition foundation (x,y)
        in if inRange foundation pos
            then Just pos
            else Nothing
    mpos _ = Nothing
    -- Quit
    cvt SDL.Quit inputs = Quit : inputs
    cvt (SDL.KeyDown (SDL.Keysym keysym _ ch)) inputs = case keysym of
        SDL.SDLK_ESCAPE -> Quit : inputs
    -- Key (pressed)
        SDL.SDLK_UP -> pressed UpArrow : inputs
        SDL.SDLK_DOWN -> pressed DownArrow : inputs
        SDL.SDLK_LEFT -> pressed LeftArrow : inputs
        SDL.SDLK_RIGHT -> pressed RightArrow : inputs
        SDL.SDLK_RETURN -> pressed Enter : inputs
        SDL.SDLK_LSHIFT -> pressed Shift : inputs
        SDL.SDLK_RSHIFT -> pressed Shift : inputs
        SDL.SDLK_LCTRL -> pressed Ctrl : inputs
        SDL.SDLK_RCTRL -> pressed Ctrl : inputs
        SDL.SDLK_LALT -> pressed AltKey : inputs
        SDL.SDLK_RALT -> pressed AltKey : inputs
        SDL.SDLK_TAB-> pressed Tab : inputs
        SDL.SDLK_BACKSPACE -> pressed Backspace : inputs
        SDL.SDLK_LSUPER -> pressed Meta : inputs
        SDL.SDLK_RSUPER -> pressed Meta : inputs
        key ->
            if (key >= SDL.SDLK_SPACE && key <= SDL.SDLK_z)
            then pressed (Char $ toLower ch) : inputs
            else inputs -- ignore
    -- Key (released)
    cvt (SDL.KeyUp (SDL.Keysym keysym _ ch)) inputs = case keysym of
        SDL.SDLK_UP -> released UpArrow : inputs
        SDL.SDLK_DOWN -> released DownArrow : inputs
        SDL.SDLK_LEFT -> released LeftArrow : inputs
        SDL.SDLK_RIGHT -> released RightArrow : inputs
        SDL.SDLK_RETURN -> released Enter : inputs
        SDL.SDLK_LSHIFT -> released Shift : inputs
        SDL.SDLK_RSHIFT -> released Shift : inputs
        SDL.SDLK_LCTRL -> released Ctrl : inputs
        SDL.SDLK_RCTRL -> released Ctrl : inputs
        SDL.SDLK_LALT -> released AltKey : inputs
        SDL.SDLK_RALT -> released AltKey : inputs
        SDL.SDLK_TAB-> released Tab : inputs
        SDL.SDLK_BACKSPACE -> released Backspace : inputs
        SDL.SDLK_LSUPER -> released Meta : inputs
        SDL.SDLK_RSUPER -> released Meta : inputs
        key ->
            if (key >= SDL.SDLK_SPACE && key <= SDL.SDLK_z)
            then released (Char $ toLower ch) : inputs
            else inputs -- ignore
    -- Click
    cvt (SDL.MouseButtonDown x y SDL.ButtonLeft) inputs = Click Location{ locX = fromIntegral x, locY = fromIntegral y } : inputs
    -- Ignore the rest
    cvt _ inputs = inputs

mergeInputs :: [Input] -> [Input] -> [Input]
mergeInputs old new = new

stepInputs :: [Input] -> [Input]
stepInputs = foldr step []
 where
    held key = Key key Held
    step inp inps = case inp of
        Key key Pressed -> held key : inps
        Key key Released -> inps
        _ -> inp : inps

getInputs :: GridLand a [Input]
getInputs = State.gets (inputs . fst)

getMousePosition :: GridLand a Location
getMousePosition = State.gets (mousePosition . fst)

pressed :: Key -> Input
pressed = flip Key Pressed

released :: Key -> Input
released = flip Key Released

rotations, colors, withoutColors, withColors, totalFrames, colorAngleInterval:: Int
rotations = 36
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

drawSpriteMapFront :: ToSprite s => Map Location s -> GridLand a ()
drawSpriteMapFront sprMap = do
    return ()

drawSpriteFront :: ToSprite s => s -> ColorFilter -> Location -> Angle -> GridLand a ()
drawSpriteFront a cf loc theta = do
    gfx <- spriteGfx (toSprite a) cf loc theta
    RWS.tell $ mempty { todoFrontSprites = Map.singleton loc gfx }

drawSpriteMiddle :: ToSprite s => s -> ColorFilter -> Location -> Angle  -> GridLand a ()
drawSpriteMiddle a cf loc theta = do
    gfx <- spriteGfx (toSprite a) cf loc theta
    RWS.tell $ mempty { todoMiddleSprites = Map.singleton loc gfx }

drawSpriteBack :: ToSprite s => s -> ColorFilter -> Location -> Angle -> GridLand a ()
drawSpriteBack a cf loc theta = do
    gfx <- spriteGfx (toSprite a) cf loc theta
    RWS.tell $ mempty { todoBackSprites = Map.singleton loc gfx }

drawSprite :: ToSprite s => s -> ColorFilter -> Location -> Angle -> GridLand a ()
drawSprite a cf loc theta = do
    (s, ts) <- RWS.asks (screen &&& tileSize)
    Gfx{..} <- spriteGfx (toSprite a) cf loc theta
    let (w, h) = (SDL.surfaceGetWidth gfxSurface, SDL.surfaceGetHeight gfxSurface)
    let tileRect = Just $ SDL.Rect ((w - ts) `div` 2) ((h - ts) `div` 2) ts ts
    void . liftIO $ SDL.blitSurface gfxSurface tileRect s (Just $ gfxRect ts gfxLocation gfxSurface)

drawGfx :: SDL.Surface -> Int -> Gfx -> IO ()
drawGfx scr ts Gfx{..} = do
    let (w, h) = (SDL.surfaceGetWidth gfxSurface, SDL.surfaceGetHeight gfxSurface)
    let tileRect = Just $ SDL.Rect ((w - ts) `div` 2) ((h - ts) `div` 2) ts ts
    void $ SDL.blitSurface gfxSurface tileRect scr (Just $ gfxRect ts gfxLocation gfxSurface)

loadMusic :: FilePath -> GridLand a Music
loadMusic rawPath = do
    path <- correctPath rawPath
    mus <- liftIO $ Mixer.loadMUS path
    key <- RWS.gets (Map.size . musics . fst)
    let music = Music key
    RWS.modify . first $ \s -> s { musics = Map.insert music mus (musics s) }
    return music

playMusic :: Music -> Maybe Int -> GridLand a ()
playMusic music mloops = do
    mus <- RWS.gets ((Map.! music) . musics . fst)
    let loops = maybe (-1) (\n -> if n < -1 then 0 else n) mloops
    liftIO $ Mixer.playMusic mus loops
    RWS.modify . first $  (\s -> s { playingMusic = Just music } )

stopMusic :: Music -> GridLand a ()
stopMusic music = do
    mplaying <- RWS.gets (playingMusic . fst)
    case mplaying of
        Nothing -> return ()
        Just currMusic -> do
            when (music == currMusic) $ do
                liftIO Mixer.haltMusic
                RWS.modify . first $ (\s -> s { playingMusic = Nothing } )

stopAllMusic :: GridLand a ()
stopAllMusic = do
    liftIO Mixer.haltMusic
    RWS.modify . first $ (\s -> s { playingMusic = Nothing } )

getPlayingMusic :: GridLand a (Maybe Music)
getPlayingMusic = RWS.gets (playingMusic . fst)

establishPlayingMusic :: GridLand a ()
establishPlayingMusic = do
    isPlaying <- liftIO Mixer.playingMusic
    unless isPlaying $ RWS.modify . first $ (\s -> s { playingMusic = Nothing } )

loadSfx :: FilePath -> GridLand a Sfx
loadSfx rawPath = do
    path <- correctPath rawPath
    sfxRef <- liftIO $ Mixer.loadWAV path
    key <- RWS.gets (Map.size . sfxs . fst)
    let sfx = Sfx key
    RWS.modify . first $ \s -> s { sfxs = Map.insert sfx sfxRef (sfxs s) }
    return sfx

playSfx :: Sfx -> GridLand a ()
playSfx sfx = do
    chunk <- RWS.gets ((Map.! sfx) . sfxs . fst)
    chan <- RWS.gets (currSfxChan . fst)
    void . liftIO $ Mixer.playChannel chan chunk 0
    RWS.modify . first $ \s -> s { currSfxChan  = mod (chan + 1) sfxChannels }

sfxChannels :: Int
sfxChannels = 16

-- | Config -> Start -> Update -> End -> IO ()
runGridLand :: Config -> GridLand () a -> GridLand a () -> GridLand a () -> IO ()
runGridLand cfg onStart onUpdate onEnd = do
    execPath <- getExecutablePath
    progName <- getProgName
    let pathPrefix = take (length execPath - length progName) execPath
    foundation@Foundation{..} <- start cfg
    let common = newCommon { pathPrefix = pathPrefix }
    (initUserData, (initCommon,_), _) <- RWS.runRWST (unGridLand onStart) foundation (common, ())
    let initState = (initCommon, initUserData)
    let update = establishPlayingMusic >> onUpdate >> drawBackdrop
    let dgfx = drawGfx screen tileSize
    endState <- ($ initState) $ fix $ \loop state -> do
        startTick <- SDL.getTicks
        let inps = (inputs . fst) state
        let pos = (mousePosition . fst) state
        (inps', mpos) <- first (mergeInputs (stepInputs inps)) <$> pollInputs foundation
        let state' = first (\s -> s {
                    inputs = inps',
                    mousePosition = fromMaybe (mousePosition s) mpos
                }) state
        (continue, state'', todo) <- RWS.runRWST (unGridLand update) foundation state'
        let ranger loc _ = inRange foundation loc
        mapM_ dgfx (Map.elems . Map.filterWithKey ranger $ todoBackSprites todo)
        mapM_ dgfx (Map.elems . Map.filterWithKey ranger $ todoMiddleSprites todo)
        mapM_ dgfx (Map.elems . Map.filterWithKey ranger $ todoFrontSprites todo)
        liftIO $ SDL.flip screen
        endTick <- SDL.getTicks
        let diff = endTick - startTick
        when (diff < 16) (SDL.delay $ 16 - diff)
        if elem Quit inps'
        then return state''
        else loop state''
    void $ RWS.execRWST (unGridLand onEnd) foundation endState
    end foundation common

inRange :: Foundation -> Location -> Bool
inRange Foundation{..} Location{..} = locX >= 0 && locY >= 0 && locX < cols && locY < rows

start :: Config -> IO Foundation
start Config{..} = do
    SDL.init [SDL.InitEverything]
    screen <- SDL.setVideoMode (cfgCols * cfgTileSize) (cfgRows * cfgTileSize) 32 [SDL.SWSurface]
    SDL.setCaption "Grid Land" ""
    SDL.enableUnicode True
    colorMap <- getColorMap screen
    -- ttfOk <- TTF.init
    Mixer.openAudio 22050 Mixer.AudioS16Sys 2 4096
    Mixer.allocateChannels sfxChannels
    return $ Foundation screen colorMap cfgRows cfgCols cfgTileSize

getColorMap :: SDL.Surface -> IO (Color -> SDL.Pixel)
getColorMap s = do
    let fmt = SDL.surfaceGetPixelFormat s
    pixels <- forM [minBound..maxBound] (\c -> liftIO $ colorValueCurry c (SDL.mapRGB fmt))
    let table = V.fromList pixels
    return $ \c -> table V.! (fromEnum c)

newCommon :: Common
newCommon = Common Map.empty Map.empty Map.empty Map.empty (BkdColor White) Nothing [] (Location 0 0) 0 ""

end :: Foundation -> Common -> IO ()
end Foundation{..} Common{..} = do
    mapM_ SDL.freeSurface (Map.elems bkdImages)
    mapM_ (mapM_ SDL.freeSurface . V.toList) (Map.elems sprites)
    mapM_ Mixer.freeMusic (Map.elems musics)
    mapM_ (const $ return ()) (Map.elems sfxs) -- Mixer.freeChunks is missing its binding
    SDL.freeSurface screen
    Mixer.closeAudio
    -- TTF.quit
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
