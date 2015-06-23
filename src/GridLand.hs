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
    ) where

-- base
import Debug.Trace
import Data.IORef
import Data.Char
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
-- array
import qualified Data.Array as Array

import GridLand.Import
import GridLand.Data

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

setColorKey :: (Word8, Word8, Word8) -> SDL.Surface -> IO SDL.Surface
setColorKey (r, g, b) sur = mapRGB sur r g b >>= SDL.setColorKey sur [SDL.SrcColorKey] >> return sur

mapRGB :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO SDL.Pixel
mapRGB = SDL.mapRGB . SDL.surfaceGetPixelFormat

colorToPixel :: Color -> SDL.Pixel
colorToPixel color = let
    (r,g,b) = colorValue color
    v = shiftL 0xff (8 * 3)  .|. shiftL (fromIntegral b) (8 * 2) .|. shiftL (fromIntegral g) (8 * 1) .|. (fromIntegral r) 
    in SDL.Pixel v

loadSpriteStretch :: FilePath -> Stretch -> GridLand a Sprite
loadSpriteStretch path stretch = do
    base <- liftIO $ Image.load path >>= SDL.displayFormat >>= setColorKey (0xff,0x00,0xff)
    let (w,h) = (SDL.surfaceGetWidth base, SDL.surfaceGetHeight base)
    let side = max w h 
    ts <- RWS.asks tileSize
    let zoom = (fromIntegral ts) / (fromIntegral side)
    let blit (cf, theta) = do
            rotozoom <- Gfx.rotozoom base (fromIntegral theta) zoom (stretch == Smooth)
            ping <- SDL.displayFormat rotozoom >>= setColorKey (0xff, 0x00, 0xff)
            pong <- setColorKey (0xff, 0x00, 0xff) rotozoom
            let (w,h) = (SDL.surfaceGetWidth rotozoom, SDL.surfaceGetHeight rotozoom)
            let fmt = SDL.surfaceGetPixelFormat rotozoom
            let cfFn v = case cf of
                    NoFilter -> SDL.Pixel v
                    Tint color -> let
                        (tr, tg, tb) = colorValue color
                        (cr, cg, cb) = fromColor32 v
                        (r, g, b) = (shiftR tr 1 + shiftR cr 1, shiftR tg 1 + shiftR cg 1, shiftR tb 1 + shiftR cb 1)
                        tinted = toColor32 r g b
                        (SDL.Pixel p) = colorToPixel color
                        in SDL.Pixel $ if 0x00ff00ff == v .&. 0x00ffffff
                            then 0x00ff00ff
                            else tinted
                    Replace color -> let
                        (SDL.Pixel p) = colorToPixel color
                        in SDL.Pixel $ if 0x00ff00ff == v .&. 0x00ffffff
                            then 0x00ff00ff
                            else p
            forM_ ([(x,y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]) $ \(x,y) -> do
                (SDL.Pixel v) <- getPixel32 x y ping
                putPixel32 x y (cfFn v) pong
            SDL.freeSurface ping
            return pong
    frames <- liftIO $ mapM blit spriteOpts
    liftIO $ SDL.freeSurface base
    let spriteFrames = V.fromListN totalFrames frames
    key <- RWS.gets (Map.size . sprites . fst)
    let spr = Sprite key
    RWS.modify . first $ \s -> s { sprites = Map.insert spr spriteFrames (sprites s) }
    return spr

loadSprite :: FilePath -> GridLand a Sprite
loadSprite path = loadSpriteStretch path Smooth

copySurface :: SDL.Surface -> IO SDL.Surface
copySurface sur = do
    copy <- copyFromSurface sur
    SDL.blitSurface sur Nothing copy Nothing
    return copy

copyFromSurface :: SDL.Surface -> IO SDL.Surface
copyFromSurface sur = do
    let fmt = SDL.surfaceGetPixelFormat sur
    bpp <- fromIntegral `liftM` SDL.pixelFormatGetBitsPerPixel fmt
    rMask <- pixelFormatGetRMask fmt
    gMask <- pixelFormatGetGMask fmt
    bMask <- pixelFormatGetBMask fmt
    flags <- SDL.surfaceGetFlags sur
    aMask <- if elem SDL.SrcColorKey flags then return 0 else pixelFormatGetAMask fmt
    SDL.createRGBSurface [SDL.SWSurface] (SDL.surfaceGetWidth sur) (SDL.surfaceGetHeight sur) bpp rMask gMask bMask aMask 

pixelFormatGetRMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetRMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 16

pixelFormatGetGMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetGMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 20

pixelFormatGetBMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetBMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 24

pixelFormatGetAMask :: SDL.PixelFormat -> IO Word32
pixelFormatGetAMask format = withForeignPtr format $ \hsc_ptr -> peekByteOff hsc_ptr 28

getPixel32 :: Int -> Int -> SDL.Surface -> IO SDL.Pixel
getPixel32 x y s = do
    pixels <- castPtr `liftM` SDL.surfaceGetPixels s
    SDL.Pixel `liftM` peekElemOff pixels ((y * SDL.surfaceGetWidth s) + x)

putPixel32 :: Int -> Int -> SDL.Pixel -> SDL.Surface -> IO ()
putPixel32 x y (SDL.Pixel pixel) s = do
    pixels <- castPtr `liftM` SDL.surfaceGetPixels s
    pokeElemOff pixels ((y * SDL.surfaceGetWidth s) + x) pixel

loadBackdropImageStretch :: FilePath -> Stretch -> GridLand a Backdrop
loadBackdropImageStretch path stretch = do
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

pollInput :: IO [Input]
pollInput = do
    events <- pollEvents
    return $ foldr cvt [] events
 where
    -- Quit
    cvt SDL.Quit inputs = Quit : inputs
    cvt (SDL.KeyDown (SDL.Keysym keysym _ ch)) inputs = case keysym of
        SDL.SDLK_ESCAPE -> Quit : inputs
    -- Key
        SDL.SDLK_UP -> pressed UpArrow : inputs
        SDL.SDLK_DOWN -> pressed DownArrow : inputs
        SDL.SDLK_LEFT -> pressed LeftArrow : inputs
        SDL.SDLK_RIGHT -> pressed RightArrow : inputs
        SDL.SDLK_RETURN -> pressed Enter : inputs
        SDL.SDLK_LSHIFT -> pressed Shift : inputs
        SDL.SDLK_RSHIFT -> pressed Shift : inputs
        SDL.SDLK_LCTRL -> pressed Ctrl : inputs
        SDL.SDLK_RCTRL -> pressed Ctrl : inputs
        SDL.SDLK_LALT -> pressed Alt : inputs
        SDL.SDLK_RALT -> pressed Alt : inputs
        SDL.SDLK_TAB-> pressed Tab : inputs
        SDL.SDLK_BACKSPACE -> pressed Backspace : inputs
        SDL.SDLK_LSUPER -> pressed Meta : inputs
        SDL.SDLK_RSUPER -> pressed Meta : inputs
        key ->
            if (key >= SDL.SDLK_SPACE && key <= SDL.SDLK_z)
            then pressed (Char $ toLower ch) : inputs
    -- Ignore
            else inputs
    cvt _ inputs = inputs

getInputs :: GridLand a [Input]
getInputs = State.gets (inputs . fst)

pressed :: Key -> Input
pressed = flip Key Pressed

fromColor32 :: Word32 -> (Word8, Word8, Word8)
fromColor32 c =
    (fromIntegral ((shiftR c (2 * 8)) .&. 0xff),
     fromIntegral ((shiftR c 8) .&. 0xff),
     fromIntegral (c .&. 0xff))

toColor32 :: Word8 -> Word8 -> Word8 -> Word32
toColor32 r g b =
    shiftL 0xff (3 * 8) .|.
    shiftL (fromIntegral b) (2 * 8) .|.
    shiftL (fromIntegral g) 8 .|.
    (fromIntegral r)

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
drawSpriteMapFront = undefined

drawSpriteFront :: ToSprite s => s -> ColorFilter -> Location -> Angle -> GridLand a ()
drawSpriteFront a cf loc theta = do
    gfx <- spriteGfx (toSprite a) cf loc theta
    RWS.tell $ mempty { todoFrontSprites = Map.singleton (locX loc, locY loc) gfx }

drawSpriteMiddle :: ToSprite s => s -> ColorFilter -> Location -> Angle  -> GridLand a ()
drawSpriteMiddle a cf loc theta = do
    gfx <- spriteGfx (toSprite a) cf loc theta
    RWS.tell $ mempty { todoMiddleSprites = Map.singleton (locX loc, locY loc) gfx }

drawSpriteBack :: ToSprite s => s -> ColorFilter -> Location -> Angle -> GridLand a ()
drawSpriteBack a cf loc theta = do
    gfx <- spriteGfx (toSprite a) cf loc theta
    RWS.tell $ mempty { todoBackSprites = Map.singleton (locX loc, locY loc) gfx }

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
loadMusic (path) = do
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

-- | (Config, Start, Update, End) -> IO ()
runGridLand :: (Config, GridLand () a, GridLand a Bool, GridLand a ()) -> IO ()
runGridLand (cfg, onStart, onUpdate, onEnd) = do
    foundation <- start cfg
    let common = newCommon
    (initUserData, (initCommon,_), _) <- RWS.runRWST (unGridLand onStart) foundation (common, ())
    let initState = (initCommon, initUserData)
    let update = establishPlayingMusic >> onUpdate >> drawBackdrop
    let dgfx = drawGfx (screen foundation) (tileSize foundation)
    endState <- ($ initState) $ fix $ \loop state -> do
        startTick <- SDL.getTicks
        inputs <- pollInput
        let state' = first (\s -> s {inputs = inputs}) state
        (continue, state'', todo) <- RWS.runRWST (unGridLand update) foundation state'
        mapM_ dgfx (Map.elems $ todoBackSprites todo)
        mapM_ dgfx (Map.elems $ todoMiddleSprites todo)
        mapM_ dgfx (Map.elems $ todoFrontSprites todo)
        liftIO $ SDL.flip (screen foundation)
        endTick <- SDL.getTicks
        let diff = endTick - startTick
        when (diff < 16) (SDL.delay $ 16 - diff)
        if elem Quit inputs
            then return state''
            else loop state''
    void $ RWS.execRWST (unGridLand onEnd) foundation endState
    end foundation common

start :: Config -> IO Foundation
start Config{..} = do
    SDL.init [SDL.InitEverything]
    screen <- SDL.setVideoMode (cfgCols * cfgTileSize) (cfgRows * cfgTileSize) 32 [SDL.SWSurface]
    SDL.setCaption "Grid Land" ""
    SDL.enableUnicode True
    colorMap <- getColorMap screen
    ttfOk <- TTF.init
    Mixer.openAudio 22050 Mixer.AudioS16Sys 2 4096
    return $ Foundation screen colorMap cfgRows cfgCols cfgTileSize

getColorMap :: SDL.Surface -> IO (Color -> SDL.Pixel)
getColorMap s = do
    let fmt = SDL.surfaceGetPixelFormat s
    pixels <- forM [minBound..maxBound] $ \c -> liftIO $ colorValue' c (SDL.mapRGB fmt)
    let table = V.fromList pixels
    return $ \c -> table V.! (fromEnum c)

newCommon :: Common
newCommon = Common Map.empty Map.empty Map.empty Map.empty (BkdColor White) Nothing []

end :: Foundation -> Common -> IO ()
end Foundation{..} Common{..} = do
    mapM_ SDL.freeSurface (Map.elems bkdImages)
    mapM_ (mapM_ SDL.freeSurface . V.toList) (Map.elems sprites)
    mapM_ Mixer.freeMusic (Map.elems musics)
    mapM_ (const $ return ()) (Map.elems sfxs) -- Mixer.freeChunks is missing a binding
    SDL.freeSurface screen
    Mixer.closeAudio
    TTF.quit
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
