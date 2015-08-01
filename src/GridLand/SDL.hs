{-# LANGUAGE CPP #-}

module GridLand.SDL where

import qualified Graphics.UI.SDL as SDL
import GridLand.Import
import GridLand.Data

setColorKey :: (Word8, Word8, Word8) -> SDL.Surface -> IO SDL.Surface
setColorKey (r, g, b) sur = mapRGB sur r g b >>= SDL.setColorKey sur [SDL.SrcColorKey] >> return sur

mapRGB :: SDL.Surface -> Word8 -> Word8 -> Word8 -> IO SDL.Pixel
mapRGB = SDL.mapRGB . SDL.surfaceGetPixelFormat

surfacePixelFormat :: SDL.Surface -> IO PixelFormat
surfacePixelFormat sur = do
    let fmt = SDL.surfaceGetPixelFormat sur
    rShift <- pixelFormatGetRShift fmt
    bShift <- pixelFormatGetBShift fmt
    gShift <- pixelFormatGetGShift fmt
    aShift <- pixelFormatGetAShift fmt
    return $ case (rShift, gShift, bShift, aShift) of
        ( 0,  8, 16, 24) -> ABGR
        (16,  8,  0, 24) -> ARGB
        (24, 16,  8,  0) -> RGBA
        ( 8, 16, 24,  0) -> BGRA
        _ -> ABGR -- default

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

pixelFormatGetBytes :: Storable a => Int -> SDL.PixelFormat -> IO a
pixelFormatGetBytes offset fmt = withForeignPtr fmt (\ptr -> peekByteOff ptr offset)

pixelFormatGetRShift, pixelFormatGetGShift, pixelFormatGetBShift, pixelFormatGetAShift :: SDL.PixelFormat -> IO Word8
#ifdef ARCH_64
pixelFormatGetRShift = pixelFormatGetBytes 14
pixelFormatGetGShift = pixelFormatGetBytes 15
pixelFormatGetBShift = pixelFormatGetBytes 16
pixelFormatGetAShift = pixelFormatGetBytes 17
#endif
#ifdef ARCH_32
pixelFormatGetRShift = pixelFormatGetBytes 10
pixelFormatGetGShift = pixelFormatGetBytes 11
pixelFormatGetBShift = pixelFormatGetBytes 12
pixelFormatGetAShift = pixelFormatGetBytes 13
#endif

pixelFormatGetRMask, pixelFormatGetGMask, pixelFormatGetBMask, pixelFormatGetAMask :: SDL.PixelFormat -> IO Word32
#ifdef ARCH_64
pixelFormatGetRMask = pixelFormatGetBytes 20
pixelFormatGetGMask = pixelFormatGetBytes 24
pixelFormatGetBMask = pixelFormatGetBytes 28
pixelFormatGetAMask = pixelFormatGetBytes 32
#endif
#ifdef ARCH_32
pixelFormatGetRMask = pixelFormatGetBytes 16
pixelFormatGetGMask = pixelFormatGetBytes 20
pixelFormatGetBMask = pixelFormatGetBytes 24
pixelFormatGetAMask = pixelFormatGetBytes 28
#endif

getPixel32 :: Int -> Int -> SDL.Surface -> IO SDL.Pixel
getPixel32 x y s = do
    pixels <- castPtr `liftM` SDL.surfaceGetPixels s
    SDL.Pixel `liftM` peekElemOff pixels ((y * SDL.surfaceGetWidth s) + x)

putPixel32 :: Int -> Int -> SDL.Pixel -> SDL.Surface -> IO ()
putPixel32 x y (SDL.Pixel pixel) s = do
    pixels <- castPtr `liftM` SDL.surfaceGetPixels s
    pokeElemOff pixels ((y * SDL.surfaceGetWidth s) + x) pixel
