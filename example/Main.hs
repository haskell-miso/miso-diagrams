{-# LANGUAGE CPP #-}

module Main where

import Data.String (IsString (fromString))
import Diagrams.Coordinates.Polar
import Diagrams.Prelude
import Miso
import Miso.Canvas qualified as Canvas
import Miso.Canvas.Diagrams
import Miso.Html.Property
import Plots
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
        renderDia Canvas (CanvasOptions absolute) . renderAxis $
            polarAxis &~ do
                let ps, ps' :: (Enum n, RealFloat n) => [Polar n]
                    ps = [mkPolar x theta | x <- [35], theta <- [20 @@ deg, 40 @@ deg .. fullTurn]]
                    ps' = (_r *~ 0.6) <$> ps

                scatterPlot ps $ key "points"
                scatterPlot ps' $ key "points'"

                legendPlacement .= rightTop
                rLabel .= "r-axis"
                thetaLabel .= "theta-axis"
  where
    w, h :: (Num a) => a
    w = 800
    h = 800
