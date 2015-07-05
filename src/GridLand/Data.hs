module GridLand.Data where

import GridLand.Import
-- SDL
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Video as SDL
-- SDL-image
import qualified Graphics.UI.SDL.Image as Image
-- SDL-ttf (unused)
--import qualified Graphics.UI.SDL.TTF as TTF
-- SDL-mixer
import qualified Graphics.UI.SDL.Mixer as Mixer

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
    | Key Key KeyState
    | Click Location
    deriving (Eq, Show)

data Key
    = Char Char
    | UpArrow
    | DownArrow
    | LeftArrow
    | RightArrow
    | Enter
    | Shift
    | Ctrl
    | Alt
    | Tab
    | Backspace
    | Meta
    deriving (Eq, Show)

data KeyState
    = Pressed
    | Held
    | Released
    deriving (Enum, Eq, Bounded, Show)

-- (1 + Tinted-N-Replace * Colors) * Rotations
newtype Sprite = Sprite { spriteKey :: Int }
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
} deriving (Show, Eq)

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
    sprites :: Map Sprite (Vector SDL.Surface),
    sfxs :: Map Sfx Mixer.Chunk,
    musics :: Map Music Mixer.Music,
    currBkd :: Backdrop,
    playingMusic :: Maybe Music,
    inputs :: [Input],
    mousePosition :: Location
}

data Todo = Todo {
    todoFrontSprites :: Map Location Gfx,
    todoMiddleSprites :: Map Location Gfx,
    todoBackSprites :: Map Location Gfx,
    todoBackdrop :: Backdrop
}

newtype GridLand a b = GridLand { unGridLand :: RWST Foundation Todo(Common, a) IO b }
    deriving
        (Functor, Applicative, Monad, MonadIO, MonadRWS Foundation Todo (Common, a)
        ,MonadReader Foundation, MonadWriter Todo, MonadState (Common, a))

class ToSprite a where
    toSprite :: a -> Sprite

instance Ord Location where
    compare loc0 loc1 = compare (locX loc0, locY loc0) (locX loc1, locY loc1)

instance ToSprite Sprite where
    toSprite = id

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
