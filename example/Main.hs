{-# LANGUAGE CPP #-}

module Main where

import Data.String (IsString (fromString))
import Diagrams.Prelude
import Graphics.Rendering.Chart.Backend.Diagrams qualified as Chart
import Graphics.Rendering.Chart.Easy qualified as Chart
import Miso
import Miso.Canvas qualified as Canvas
import Miso.Canvas.Diagrams
import Miso.Html.Property
import System.IO.Unsafe (unsafePerformIO)
import Prelude

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

data Model = Model
    deriving stock (Eq)

emptyModel :: Model
emptyModel = Model

data Action = Setup

main :: IO ()
main = run $ startApp app

app :: App Model Action
app = do
    initComponent
        { events = defaultEvents <> keyboardEvents
        , initialAction = Just Setup
        , styles = []
        }
  where
    initComponent :: Component ROOT Model Action
    initComponent = component emptyModel appUpdate appView

appUpdate :: Action -> Effect ROOT Model Action
appUpdate Setup = pure ()

appView :: Model -> View Model Action
appView _model =
    Canvas.canvas [width_ (fromString . show @Int $ w), height_ (fromString . show @Int $ h)] (const $ pure ()) \() ->
        renderDia Canvas (CanvasOptions absolute) . fst . Chart.runBackendR env . Chart.toRenderable . Chart.execEC $ do
            Chart.layout_title .= "Amplitude Modulation"
            Chart.plot (Chart.line "am" [signal [0, (0.5) .. 400]])
            Chart.plot (Chart.points "am points" (signal [0, 7 .. 400]))
  where
    w, h :: (Num a) => a
    w = 800
    h = 300
    env :: Chart.DEnv Double
    env = unsafePerformIO $ Chart.defaultEnv Chart.vectorAlignmentFns w h
    signal :: [Double] -> [(Double, Double)]
    signal xs = [(x, (sin (x * pi / 45) + 1) / 2 * (sin (x * pi / 5))) | x <- xs]
