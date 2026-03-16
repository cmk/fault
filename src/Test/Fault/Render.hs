{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Fault.Render (
    Pos (..)
  , srcFile
  , Pointer (..)
  , pointer

  , viaShow
  , Ansi
  , Doc (..)

  , code
  , blink
  , bold
  , italic
  , underline
  , faint
  , dull
  , vivid
  , layer
  , palette
  , green
  , yellow
  , red
  , Color(..)
  , XColor
  , Palette
  , ConsoleLayer(..)

  , defaultLayoutOptions
  , layoutPretty
  , putDoc
  , renderAnsi
  , renderHTML
  , renderStrict
  , renderText
  , renderPlain
  , Pretty (..)
) where

import Control.DeepSeq (NFData (..), ($!!))
import Control.Monad.IO.Class
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.Typeable
import Data.Word
import GHC.Arr as A hiding (fill)
import GHC.Generics
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import Prelude as P
import Prettyprinter as Pretty hiding (width, pipe, column, line)
import Prettyprinter.Render.Text
import Prettyprinter.Render.Util.StackMachine
import System.Console.ANSI.Codes
import System.Console.ANSI.Types -- (SGR, Color(..))
import System.IO
import System.IO.Error (userError)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.IO as T

import Control.Exception (IOException, handle)
import Control.Monad.Except

{-
renderSGR = renderAnsi defaultLayoutOptions id id


ol :: T.Text -> IO ()
ol x = runExceptT mkCaret >>= pure . either (const mempty) (\(a,b) -> overlayPointer a x b) >>= T.putStrLn . renderSGR . report

mkCaret :: MonadIO m => ExceptT IOException m (Pointer, T.Text)
mkCaret = do
{-
  loc <- MaybeT $ pure callSite
  src <- MaybeT $ readFileSafe (srcLocFile loc)
  let
      l = srcLocStartRow loc - 1 
      c = srcLocStartCol loc - 1
      w = srcLocEndCol loc - 1 - c
      x = lines src !! l
  return $ (Caret (Loc l c w) lbl True, T.pack x)
-}

  (path, loc) <- liftEither $ maybe (Left err) Right $ srcLoc
  src <- readFileSafe path
  let
      x = lines src !! row loc
  return $ (loc, T.pack x)

  where
    err = userError "Failed to locate file"
-}

ol :: Label -> IO ()
ol l = do 
  m <- readSrcFile a
  T.putStrLn . renderSGR . report . maybe mempty id $ overlay <$> a <*> m
  where
    a = pointer l
    overlay p t = overlayPointer p $ T.lines t !! row (start p)

renderSGR = renderAnsi defaultLayoutOptions id id



-- | Row number, starts at 0.
type Row = Int

-- | Column number, starts at 0.
type Col = Int

-- | Affine distance between two columns.
type Width = Int

-- | Label text.
type Label = T.Text

-- Pos

-------------------------

-- | Position in a source file.
data Pos = Pos {row :: !Row, col :: !Col}
    deriving (Eq, Ord, Generic, Typeable)

instance NFData Pos

instance Show Pos where
    showsPrec _ (Pos l c) = shows l . (':' :) . shows c

--instance ToLogStr Pos where
--    toLogStr = toLogStr . show

instance Semigroup Pos where
    Pos r1 c1 <> Pos r2 c2 = Pos (r1 + r2) (c1 + c2)

instance Monoid Pos where
    mempty = Pos 0 0

-- Pointer

-------------------------

{- | An annotated line of source code.

 ex = overlay (0, 35) "int main(int argc, char ** argv) { int; }"
 >>> unAnnotate $ report (drawOverlay (0, 35) (0, 38) .% ex)
 1 | int main(int argc, char ** argv) { int; }
   |                                    ~~~
-}
data Pointer = Pointer
    { -- | The path of the annotated file.
      path :: !FilePath
    , -- | The starting position of the pointer.
      start :: !Pos
    , -- | The width of the pointer.
      width :: !Width
    , -- | An optional comment for the pointer.
      label :: !Label
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Pointer

pointer :: HasCallStack => Label -> Maybe Pointer
pointer l = mkPointer l <$> srcLoc

mkPointer :: Label -> SrcLoc -> Pointer
mkPointer l x = Pointer fp (Pos ls cs) (if ls == le then cw else 0) l
  where
    fp = srcLocFile x
    ls = srcLocStartLine x - 1
    le = srcLocEndLine x - 1
    cs = srcLocStartCol x - 1
    cw = srcLocEndCol x - srcLocStartCol x

--sameRow :: Loc a -> Loc b -> Bool
sameRow la lb = row (start la) == row lb

-- | ANSI terminal style for source the pointer.
pointerEffects :: [SGR]
pointerEffects = [SetColor Foreground Vivid Red]

drawPointer_ :: Pointer -> Pos -> Src -> Src
drawPointer_ ptr pos src =
  if sameRow ptr pos
    then f (col $ start ptr) src $ T.replicate (max (width ptr) 0) $ T.pack ['~']
    else src
  --  sameRow l x                 = f (snd l) src $ T.replicate (max (snd u - snd l + 1) 0) c
  --                  sameRow h x = f (-1)    src $ T.replicate (max (snd h + 1) 0) c
  where
    -- l = min start end
    -- h = max start end
    -- u = snd (bounds src)
    -- c = T.pack ['~']
    f = flip . draw pointerEffects 1

drawPointer :: Pointer -> Pos -> Src -> Src
drawPointer ptr d a = draw pointerEffects 2 (col $ start ptr) (label ptr) $ drawPointer_ ptr d a

-- | Render a pointer at a certain position in a 'Overlay'.
overlayPointer :: Pointer -> T.Text -> Overlay
overlayPointer ptr ln =
  if label ptr == mempty
    then drawPointer_ ptr .# overlay (start ptr) ln
    else drawPointer ptr .# overlay (start ptr) ln

-- | Add call site information to a test.
srcLoc :: HasCallStack => Maybe SrcLoc
srcLoc = fmap snd $ listToMaybe (reverse . getCallStack $ callStack)

srcFile :: HasCallStack => Maybe FilePath
srcFile = fmap srcLocFile srcLoc

readSrcFile :: MonadIO m => Maybe Pointer -> m (Maybe T.Text)
readSrcFile m = liftIO $ do
    case m of
        Nothing -> pure $ Just "<< interactive >>"
        Just ann ->
            handle
                (\(_ :: IOException) -> pure Nothing)
                (Just <$> T.readFile (path ann))

-- Src

-------------------------

-- | A raw canvas to paint ANSI-styled characters on.
type Src = Array (Row, Col) ([SGR], Char)

-- | Remove a number of @(index, element)@ values from an @'Array'@.
(///) :: Ix i => Array i e -> [(i, e)] -> Array i e
a /// xs = a // filter (inRange (bounds a) . fst) xs

draw :: [SGR] -> Row -> Col -> T.Text -> Src -> Src
draw sgr row col xs a0 = if T.null xs then a0 else gt $ lt (a /// out)
  where
    a = grow row a0

    ((_,lo),(_,hi)) = bounds a -- col low and high

    out = zipWith (\i c -> ((row,i),(sgr,c))) [col..] $ T.unpack xs

    column = snd . fst

    lt | any ((lo >) . column) out = (// [((row,lo),(outOfRangeEffects sgr,'<'))])
       | otherwise = id

    gt | any ((hi <) . column) out = (// [((row,hi),(outOfRangeEffects sgr,'>'))])
       | otherwise = id

    --outOfRangeEffects :: [SGR] -> [SGR]
    outOfRangeEffects xs = SetConsoleIntensity BoldIntensity : xs

-- extend /a/ so that it includes row /y/
grow :: Row -> Src -> Src
grow r a
  | inRange (t,b) r = a
  | otherwise = array new [ (i, if inRange old i then a ! i else ([],' ')) | i <- range new ]
  where old@((t,lo),(b,hi)) = bounds a
        new = ((min t r,lo),(max b r,hi))


-- Overlay

-------------------------

-- | ANSI terminal style for source the gutter.
gutterEffects :: [SGR]
gutterEffects = [SetColor Foreground Vivid Yellow]

-- | An 'Overlay' is a canvas of text that output can be written to.
data Overlay = Overlay !Pos !Int (Src -> Src) (Pos -> Src -> Src)

instance Show Overlay where
    showsPrec d (Overlay pos len _ _) =
        showParen (d > 10) $
            showString "Overlay " . showsPrec 11 pos . showChar ' ' . showsPrec 11 len . showChar ' ' . showString "... ..."

instance Semigroup Overlay where
    o1@(Overlay p1 l1 _ _) <> o2@(Overlay p2 l2 _ _) = if (p1, l1) > (p2, l2) then o1 else o2

instance Monoid Overlay where
    mempty = Overlay mempty 0 id (const id)

-- | Create an 'Overlay' from a row of text
--
-- Anything after a newline is dropped
overlay :: Pos -> T.Text -> Overlay
overlay pos s0 = Overlay pos (T.length s) (draw [] 0 0 s) (const id)
  where
    --overlay pos s0 = Overlay (l <$ pos) (draw [] 0 l mempty . draw [] 0 0 s) (const id)
    --  T.any (=='\n') s0 = Overlay pos ls (draw [] 0 0 s) (const id)
    --  otherwise = Overlay pos ls (draw [] 0 ls mempty . draw [] 0 0 s) (const id)

    s = go 0 s0

    -- l = T.length s

    space = T.pack [' ']

    go n t = case T.uncons t of
        Just ('\t', xs) -> let t = 8 - mod n 8 in T.replicate t space <> go (n + t) xs
        Just ('\n', xs) -> mempty
        Just (x, xs) -> x `T.cons` go (n + 1) xs
        Nothing -> mempty

(.#) :: (Pos -> Src -> Src) -> Overlay -> Overlay
f .# Overlay p l s g = Overlay p l s $ \x y -> f x $ g x y

 
report :: Overlay -> Ansi
report o =
  nesting $ \k ->
    columns $ \mn ->
      reportWidth (fromMaybe 80 mn - k) o


reportWidth :: Int -> Overlay -> Ansi
reportWidth cols (Overlay pos len f g) = align $ vsep $ fmap ppRow [x..y]
  where
    (lo, hi) = window (col pos) len $ min (max (cols - 5 - gutterWidth) 30) 200
    a = g pos $ f $ array ((0,lo),(-1,hi)) []
    ((x,_),(y,_)) = bounds a
    n = show $ 1 + row pos
    gutterWidth = P.length n
    gutter = pretty n <+> pipe
    margin = fill gutterWidth space <+> pipe
    h x y = annotate gutterEffects (if x == 0 then gutter else margin) <+> y
    ppRow x = h x $ hcat
                   $ fmap (annotate <$> fst . P.head <*> pretty . fmap snd)
                   $ groupBy ((==) `on` fst)
                   [ a ! (x,i) | i <- [lo..hi] ]

window :: Col -> Int -> Int -> (Int, Int)
window c l w
  | c <= w2     = (0, min w l)
  | c + w2 >= l = if l > w then (l-w, l) else (0, w)
  | otherwise   = (c-w2, c+w2)
  where w2 = div w 2

columns :: (Maybe Int -> Ansi) -> Ansi
columns f = pageWidth (f . toMaybeInt) where
  toMaybeInt (AvailablePerLine cpl _) = Just cpl
  toMaybeInt Unbounded = Nothing

renderOverlay :: Overlay -> Int -> Row -> [([SGR], Char)]
renderOverlay (Overlay pos len f g) n x = [src ! (x, i) | i <- [lo .. hi]]
  where
    (lo, hi) = window (col pos) len n
    src = g pos $ f $ array ((0, lo), (-1, hi)) []

-- Ansi

-------------------------

type Ansi = Doc [SGR]

-- | Render a 'Stream', discarding all pointers.
renderPlain :: Monoid o => LayoutOptions -> (T.Text -> o) -> Doc a -> o
renderPlain opts f = renderSimplyDecorated f (const mempty) (const mempty) . layoutSmart opts

renderText :: Doc a -> TL.Text
renderText = TB.toLazyText . renderPlain defaultLayoutOptions TB.fromText

-- | Display a rendered document with ANSI escape sequences.
--
renderAnsi :: Monoid o => LayoutOptions -> (T.Text -> o) -> (a -> [SGR]) -> Doc a -> o
renderAnsi opts f g = renderSimplyDecorated f i o . layoutSmart opts
  where
    i = (f . T.pack . setSGRCode . g)
    o = (const $ f $ T.pack $ setSGRCode [System.Console.ANSI.Codes.Reset])

{- | Display a rendered document as HTML and output a 'Monoid'.
 The annotated region is wrapped by @<span class="f a">..</span>@ with the @class@ attribute
 given by the pointer function.
-}
renderHTML :: Monoid o => LayoutOptions -> (T.Text -> o) -> (a -> T.Text) -> Doc a -> o
renderHTML opts f g = renderSimplyDecorated txt push pop . layoutSmart opts
  where
    push t = f "<span class=\"" <> f (g t) <> f "\">"
    pop = const $ f "</span>"
    txt = f . escapeHTML

    -- Escape an HTML string by replacing special characters with HTML entities.
    --escapeHTML :: Text -> Text
    escapeHTML = T.concatMap $ \c ->
        case c of
            '"' -> "&quot;"
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            _ -> T.singleton c


code :: SGR -> Ansi -> Ansi
code = annotate . pure

-- Emphasis

-------------------------

blink :: Ansi -> Ansi
blink = code $ SetBlinkSpeed SlowBlink

bold :: Ansi -> Ansi
bold = code $ SetConsoleIntensity BoldIntensity

italic :: Ansi -> Ansi
italic = code $ SetItalicized True

underline :: Ansi -> Ansi
underline = code $ SetUnderlining SingleUnderline

faint :: Ansi -> Ansi
faint = code $ SetConsoleIntensity FaintIntensity

-- Color

-------------------------

-- | The xterm < https://en.wikipedia.org/wiki/8-bit_color 8 bit > color encoding.
type XColor = Word8

-- | A simple palette consisting of a foreground and background color.
type Palette = (XColor, XColor)

dull :: Color -> ConsoleLayer -> Ansi -> Ansi
dull = layer . xtermSystem Dull

vivid :: Color -> ConsoleLayer -> Ansi -> Ansi
vivid = layer . xtermSystem Vivid

--vivid col lay = code $ SetColor lay Vivid col

layer :: XColor -> ConsoleLayer -> Ansi -> Ansi
layer pal lay = code $ SetPaletteColor lay pal

palette :: Palette -> Ansi -> Ansi
palette (fg, bg) = annotate [SetPaletteColor Foreground fg, SetPaletteColor Background bg]


-- Color

-------------------------

green :: Ansi -> Ansi
green = vivid Green Foreground
{-# INLINE green #-}

yellow :: Ansi -> Ansi
yellow = vivid Yellow Foreground
{-# INLINE yellow #-}

red :: Ansi -> Ansi
red = vivid Red Foreground
{-# INLINE red #-}



ppLocation fp (l, c) = arrow <+> pretty fp <> colon <> pretty l <> colon <> pretty c 

prefix padding = fill padding space <+> pipe <> space

omitPrefix padding = dot <> fill padding space <+> pipe <> space

-- TODO use align/hang etc
linePrefix n = pretty @Int n <+> pipe <> space

text = pretty @T.Text
--line = const id
arrow = text "→"
--caret = text "^"
dash = text "─"
pipe = text "│"

upRight = text "└"
downRight = text "┌"
upDownRight = text "├"


-- Emoji

-------------------------

poop :: Doc ann
poop = text "\128169"

hundred :: Doc ann
hundred = text "\128175"

monocle :: Doc ann
monocle = text "\129488"

thinking :: Doc ann
thinking = text "\129300"

grinning :: Doc ann
grinning = text "\128512"

upsideDown :: Doc ann
upsideDown = text "\128579"

thumbsUp :: Doc ann
thumbsUp = text "\128077"

thumbsDown :: Doc ann
thumbsDown = text "\128078"
