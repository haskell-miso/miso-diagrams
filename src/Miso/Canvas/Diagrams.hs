{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Miso.Canvas.Diagrams where

import Control.Lens.Operators hiding ((#))
import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State, evalState)
import Control.Monad.StateStack (StateStackT, evalStateStackT)
import Control.Monad.StateStack qualified as StateStack
import Control.Monad.Trans (lift)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import Data.NumInstances ()
import Data.Text qualified as Text
import Data.Tree (Tree (Node))
import Data.Word (Word8)
import Diagrams.Core.Transform (matrixHomRep)
import Diagrams.Core.Types (Annotation, RNode (..), RTree)
import Diagrams.Prelude hiding (fillTexture, moveTo, size, stroke)
import Diagrams.TwoD.Adjust (adjustDia2D)
import Diagrams.TwoD.Attributes (splitTextureFills)
import Diagrams.TwoD.Path (Clip (Clip))
import Diagrams.TwoD.Text (FontSlant (..), FontWeight (..), Text (..), TextAlignment (..), getFont, getFontSize, getFontSlant, getFontWeight)
import Language.Javascript.JSaddle (JSM, MakeArgs, liftJSM)
import Miso (newImage, toMisoString)
import Miso.CSS.Color qualified as Miso
import Miso.Canvas qualified as Miso
import Prelude

{- | This data declaration is simply used as a token to distinguish
  this rendering engine.
-}
data Canvas = Canvas
    deriving stock (Eq, Ord, Read, Show)

type instance V Canvas = V2

type instance N Canvas = Double

data CanvasState = CanvasState
    { _accumStyle :: Style V2 Double
    , _csPos :: (Double, Double)
    }

makeLenses ''CanvasState

instance Default CanvasState where
    def =
        CanvasState
            { _accumStyle = mempty
            , _csPos = (0, 0)
            }

type RenderM a = StateStackT CanvasState (ReaderT Miso.CanvasContext2D JSM) a

liftC :: Miso.Canvas a -> RenderM a
liftC = lift

runRenderM :: RenderM a -> Miso.Canvas a
runRenderM = flip evalStateStackT def

instance Semigroup (Render Canvas V2 Double) where
    C c1 <> C c2 = C (c1 >> c2)

instance Monoid (Render Canvas V2 Double) where
    mempty = C $ pure ()

instance Backend Canvas V2 Double where
    data Render Canvas V2 Double = C (RenderM ())
    type Result Canvas V2 Double = Miso.Canvas ()
    data Options Canvas V2 Double = CanvasOptions
        { _canvasSize :: SizeSpec V2 Double
        }

    renderRTree
        :: Canvas
        -> Options Canvas V2 Double
        -> RTree Canvas V2 Double Annotation
        -> Result Canvas V2 Double
    renderRTree _ _ rt = evalState canvasOutput initialCanvasRenderState
      where
        canvasOutput :: State CanvasRenderState (Miso.Canvas ())
        canvasOutput = do
            let C r = toRender rt
            pure . runRenderM $ r

    adjustDia c opts d = adjustDia2D size c opts (d # reflectY)

runC :: Render Canvas V2 Double -> RenderM ()
runC (C r) = r

toRender :: RTree Canvas V2 Double Annotation -> Render Canvas V2 Double
toRender =
    fromRTree
        . Node (RStyle (mempty # recommendFillColor (transparent :: AlphaColour Double)))
        . (: [])
        . splitTextureFills
  where
    fromRTree :: (Renderable (Prim b V2 Double) Canvas) => Tree (RNode b V2 Double a) -> Render Canvas V2 Double
    fromRTree (Node (RPrim p) _) = render Canvas p
    fromRTree (Node (RStyle sty) rs) = C $ do
        save
        canvasStyle sty
        accumStyle %= (<> sty)
        runC $ foldMap fromRTree rs
        restore
    fromRTree (Node _ rs) = foldMap fromRTree rs

data CanvasRenderState = CanvasRenderState

initialCanvasRenderState :: CanvasRenderState
initialCanvasRenderState = CanvasRenderState

getSize :: Options Canvas V2 Double -> SizeSpec V2 Double
getSize (CanvasOptions{_canvasSize = s}) = s

setSize :: Options Canvas V2 Double -> SizeSpec V2 Double -> Options Canvas V2 Double
setSize o s = o{_canvasSize = s}

size :: Lens' (Options Canvas V2 Double) (SizeSpec V2 Double)
size = lens getSize setSize

move :: (Double, Double) -> RenderM ()
move p = do csPos .= p

save :: RenderM ()
save = StateStack.save >> liftC (Miso.save ())

restore :: RenderM ()
restore = liftC (Miso.restore ()) >> StateStack.restore

newPath :: RenderM ()
newPath = liftC $ Miso.beginPath ()

closePath :: RenderM ()
closePath = liftC $ Miso.closePath ()

moveTo :: Double -> Double -> RenderM ()
moveTo x y = do
    liftC $ Miso.moveTo (x, y)
    move (x, y)

relLineTo :: Double -> Double -> RenderM ()
relLineTo x y = do
    p <- use csPos
    let p' = p + (x, y)
    liftC $ Miso.lineTo p'
    move p'

relCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> RenderM ()
relCurveTo ax ay bx by cx cy = do
    p <- use csPos
    let (ax', ay') = p + (ax, ay)
        (bx', by') = p + (bx, by)
        (cx', cy') = p + (cx, cy)
    liftC $ Miso.bezierCurveTo (ax', ay', bx', by', cx', cy')
    move (cx', cy')

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: (AttributeClass a) => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = fmap f . getAttr <$> use accumStyle

{- | From the HTML5 canvas specification regarding line width:

    "On setting, zero, negative, infinite, and NaN values must be
    ignored, leaving the value unchanged; other values must change
    the current value to the new value.

  Hence we must implement a line width of zero by simply not
  sending a stroke command.
-}
stroke :: RenderM ()
stroke = do
    -- The default value of 0.5 is somewhat arbitary since lineWidth should never
    -- be 'Nothing'. 0.5 is choose since it is the lower bound of the
    -- default.
    w <- fromMaybe 0.5 <$> getStyleAttrib getLineWidth
    when (w > (0 :: Double)) (liftC $ Miso.stroke ())

fill :: RenderM ()
fill = liftC $ Miso.fill ()

clip :: RenderM ()
clip = liftC $ Miso.clip ()

byteRange :: Double -> Word8
byteRange d = floor (d * 255)

texture :: (forall args. (MakeArgs args) => args -> Miso.Canvas ()) -> Texture Double -> Double -> RenderM ()
texture styleFn (SC (SomeColor c)) o = liftC . styleFn $ s
  where
    s = showColorJS c o
texture styleFn (LG g) _ = liftC $ do
    grd <- Miso.createLinearGradient (x0, y0, x1, y1)
    mapM_ (\(stop, c) -> Miso.addColorStop (stop, c) grd) stops
    styleFn grd
  where
    (x0, y0) = unp2 $ transform (g ^. lGradTrans) (g ^. lGradStart)
    (x1, y1) = unp2 $ transform (g ^. lGradTrans) (g ^. lGradEnd)
    stops = map (\s -> (s ^. stopFraction, showColorJS (s ^. stopColor) 1)) (g ^. lGradStops)
texture styleFn (RG g) _ = liftC $ do
    grd <- Miso.createRadialGradient (x0, y0, r0, x1, y1, r1)
    mapM_ (\(stop, c) -> Miso.addColorStop (stop, c) grd) stops
    styleFn grd
  where
    (r0, r1) = (s * g ^. rGradRadius0, s * g ^. rGradRadius1)
    (x0, y0) = unp2 $ transform (g ^. rGradTrans) (g ^. rGradCenter0)
    (x1, y1) = unp2 $ transform (g ^. rGradTrans) (g ^. rGradCenter1)
    stops = map (\st -> (st ^. stopFraction, showColorJS (st ^. stopColor) 1)) (g ^. rGradStops)
    s = avgScale $ g ^. rGradTrans

showColorJS :: (Color c) => c -> Double -> Miso.Color
showColorJS c o = Miso.rgba (i r) (i g) (i b) (a * o)
  where
    i :: Double -> Int
    i = fromIntegral . byteRange
    (r, g, b, a) = colorToSRGBA . toAlphaColour $ c

canvasTransform :: T2 Double -> RenderM ()
canvasTransform tr
    | [[ax, ay], [bx, by], [tx, ty]] <- matrixHomRep tr = liftC . Miso.transform $ (ax, ay, bx, by, tx, ty)
    | otherwise = error "canvasTransform: unexpected output from matrixHomeRep"

strokeTexture :: Texture Double -> Double -> RenderM ()
strokeTexture = texture $ Miso.set "strokeStyle"

fillTexture :: Texture Double -> Double -> RenderM ()
fillTexture = texture $ Miso.set "fillStyle"

fromLineCap :: LineCap -> Miso.LineCapType
fromLineCap LineCapRound = Miso.LineCapRound
fromLineCap LineCapSquare = Miso.LineCapSquare
fromLineCap LineCapButt = Miso.LineCapButt

fromLineJoin :: LineJoin -> Miso.LineJoinType
fromLineJoin LineJoinRound = Miso.LineJoinRound
fromLineJoin LineJoinBevel = Miso.LineJoinBevel
fromLineJoin LineJoinMiter = Miso.LineJoinMiter

showFontJS :: FontWeight -> FontSlant -> Double -> String -> Text.Text
showFontJS wgt slant sz fnt = Text.concat [a, " ", b, " ", c, " ", d]
  where
    a = case wgt of
        FontWeightNormal -> ""
        FontWeightBold -> "bold"
        _ -> ""
    b = case slant of
        FontSlantNormal -> ""
        FontSlantItalic -> "italic"
        FontSlantOblique -> "oblique"
    c = Text.concat [Text.pack $ show sz, "pt"]
    d = Text.pack fnt

renderC :: (Renderable a Canvas, V a ~ V2, N a ~ Double) => a -> RenderM ()
renderC a = let C r = render Canvas a in r

canvasStyle :: Style v Double -> RenderM ()
canvasStyle s =
    sequence_
        . catMaybes
        $ [ handle clip'
          , handle lWidth
          , handle lCap
          , handle lJoin
          ]
  where
    handle :: (AttributeClass a) => (a -> RenderM ()) -> Maybe (RenderM ())
    handle f = f <$> getAttr s
    clip' = mapM_ (\p -> canvasPath p >> clip) . op Clip
    lWidth = liftC . Miso.lineWidth . getLineWidth
    lCap = liftC . Miso.lineCap . fromLineCap . getLineCap
    lJoin = liftC . Miso.lineJoin . fromLineJoin . getLineJoin

instance Renderable (Segment Closed V2 Double) Canvas where
    render _ (Linear (OffsetClosed (V2 x y))) = C $ relLineTo x y
    render
        _
        ( Cubic
                (V2 x1 y1)
                (V2 x2 y2)
                (OffsetClosed (V2 x3 y3))
            ) =
            C $ relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) Canvas where
    render _ = withTrail renderLine renderLoop
      where
        renderLine ln = C $ do
            mapM_ renderC (lineSegments ln)
        renderLoop lp = C $ do
            case loopSegments lp of
                (segs, Linear _) -> mapM_ renderC segs
                _ -> mapM_ renderC (lineSegments . cutLoop $ lp)
            closePath

instance Renderable (Path V2 Double) Canvas where
    render _ p = C $ do
        canvasPath p
        f <- getStyleAttrib getFillTexture
        s <- getStyleAttrib getLineTexture
        o <- fromMaybe 1 <$> getStyleAttrib getOpacity
        save
        when (isJust f) (fillTexture (fromJust f) o >> fill)
        strokeTexture (fromMaybe (SC (SomeColor (black :: Colour Double))) s) o
        stroke
        restore

-- Add a path to the Canvas context, without stroking or filling it.
canvasPath :: Path V2 Double -> RenderM ()
canvasPath (Path trs) = do
    newPath
    forM_ trs $ \(viewLoc -> (unp2 -> p, tr)) -> do
        uncurry moveTo p
        renderC tr

instance Renderable (Text Double) Canvas where
    render _ (Text tr al str) = C $ do
        tf <- fromMaybe "Calibri" <$> getStyleAttrib getFont
        sz <- fromMaybe 12 <$> getStyleAttrib getFontSize
        slant <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
        fw <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
        tx <-
            fromMaybe (SC (SomeColor (black :: Colour Double)))
                <$> getStyleAttrib getFillTexture
        o <- fromMaybe 1 <$> getStyleAttrib getOpacity
        let fSize = avgScale tr * sz
            fnt = showFontJS fw slant fSize tf
            vAlign = case al of
                BaselineText -> Miso.TextBaselineAlphabetic
                BoxAlignedText _ h -> case h of
                    h' | h' <= 0.25 -> Miso.TextBaselineBottom
                    h' | h' >= 0.75 -> Miso.TextBaselineTop
                    _ -> Miso.TextBaselineMiddle
            hAlign = case al of
                BaselineText -> Miso.TextAlignStart
                BoxAlignedText w _ -> case w of
                    w' | w' <= 0.25 -> Miso.TextAlignStart
                    w' | w' >= 0.75 -> Miso.TextAlignEnd
                    _ -> Miso.TextAlignCenter
        save
        liftC $ Miso.textBaseline vAlign
        liftC $ Miso.textAlign hAlign
        liftC . Miso.font . toMisoString . Text.unpack $ fnt
        fillTexture tx o
        canvasTransform (tr <> reflectionY)
        liftC $ Miso.fillText (toMisoString str, 0, 0)
        restore

instance Renderable (DImage Double External) Canvas where
    render _ (DImage path w' h' tr) = C $ do
        let ImageRef file = path
        save
        canvasTransform (tr <> reflectionY)
        img <- liftC . liftJSM . Miso.newImage . toMisoString $ file
        let w = fromIntegral w'
            h = fromIntegral h'
            x = - (w / 2)
            y = - (h / 2)
        liftC $ Miso.drawImage' (img, x, y, w, h)
        restore
