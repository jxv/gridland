module GridLand.Color where

import qualified Graphics.UI.SDL as SDL
import GridLand.Import
import GridLand.Data
import GridLand.SDL

colorToPixel :: Color -> SDL.Pixel
colorToPixel color = let
    (r',g,b') = colorValue color
    (r,b) = (b',r')
    v = shiftL 0xff (8 * 3)  .|. shiftL (fromIntegral b) (8 * 2) .|. shiftL (fromIntegral g) (8 * 1) .|. (fromIntegral r)
    in SDL.Pixel v

fromColorARGB :: Word32 -> (Word8, Word8, Word8)
fromColorARGB c =
    (fromIntegral ((shiftR c (2 * 8)) .&. 0xff),
     fromIntegral ((shiftR c 8) .&. 0xff),
     fromIntegral (c .&. 0xff))

fromColorABGR :: Word32 -> (Word8, Word8, Word8)
fromColorABGR c =
    (fromIntegral (c .&. 0xff),
     fromIntegral ((shiftR c 8) .&. 0xff),
     fromIntegral ((shiftR c (2 * 8)) .&. 0xff))

fromColorRGBA :: Word32 -> (Word8, Word8, Word8)
fromColorRGBA c =
    (fromIntegral ((shiftR c (3 * 8)) .&. 0xff),
     fromIntegral ((shiftR c (2 * 8)) .&. 0xff),
     fromIntegral ((shiftR c 8) .&. 0xff))

fromColorBGRA :: Word32 -> (Word8, Word8, Word8)
fromColorBGRA c =
    (fromIntegral ((shiftR c 8) .&. 0xff),
     fromIntegral ((shiftR c (2 * 8)) .&. 0xff),
     fromIntegral ((shiftR c (3 * 8)) .&. 0xff))

toColorABGR :: Word8 -> Word8 -> Word8 -> Word32
toColorABGR r g b =
    shiftL 0xff (3 * 8) .|.
    shiftL (fromIntegral b) (2 * 8) .|.
    shiftL (fromIntegral g) 8 .|.
    (fromIntegral r)

toColorARGB :: Word8 -> Word8 -> Word8 -> Word32
toColorARGB r g b =
    shiftL 0xff (3 * 8) .|.
    shiftL (fromIntegral r) (2 * 8) .|.
    shiftL (fromIntegral g) 8 .|.
    (fromIntegral b)

toColorBGRA :: Word8 -> Word8 -> Word8 -> Word32
toColorBGRA r g b =
    shiftL (fromIntegral b) (3 * 8) .|.
    shiftL (fromIntegral g) (2 * 8) .|.
    shiftL (fromIntegral r) 8 .|.
    0xff

toColorRGBA :: Word8 -> Word8 -> Word8 -> Word32
toColorRGBA r g b =
    shiftL (fromIntegral r) (3 * 8) .|.
    shiftL (fromIntegral g) (2 * 8) .|.
    shiftL (fromIntegral b) 8 .|.
    0xff

colorValue :: Integral a => Color -> (a, a, a)
colorValue Red = (0xcc, 0x00, 0x00)
colorValue Orange = (0xff, 0xa5, 0x00)
colorValue Yellow = (0xff, 0xff, 0x00)
colorValue YellowGreen = (0x7f, 0xff, 0x00)
colorValue Green = (0x00, 0x66, 0x00)
colorValue LightBlue = (0x66, 0xff, 0xff)
colorValue Blue = (0x00, 0x00, 0x99)
colorValue Pink = (0xff, 0x66, 0x99)
colorValue Black = (0x00, 0x00, 0x00)
colorValue White = (0xff, 0xff, 0xff)

colorValueCurry :: Integral a => Color -> (a -> a -> a -> b) -> b
colorValueCurry c f = uncurryN f (colorValue c)
