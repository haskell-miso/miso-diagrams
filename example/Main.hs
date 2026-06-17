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

main :: IO ()
main = startApp defaultEvents app

data Model = Model
    deriving stock (Eq)

data Action = Setup

app :: App Model Action
app = (component emptyModel appUpdate appView) 
        { mount = Just Setup 
        , styles = []
        }

emptyModel :: Model 
emptyModel = Model

appUpdate :: Action -> Effect ROOT () Model Action
appUpdate Setup = pure ()

appView :: () -> Model -> View Model Action
appView _props _model = let
    w , h :: Num a => a 
    w = 800
    h = 800

    w' = fromString . show @Int $ w
    h' = fromString . show @Int $ h 
    in Canvas.canvas [ width_ w' , height_ h' ] (const $ pure ()) \() ->
            renderDia Canvas (CanvasOptions absolute) . renderAxis $
                polarAxis &~ do
                let ps, ps' :: (Enum n, RealFloat n) => [ Polar n ]
                    ps = [ mkPolar x theta | x <- [ 35 ], theta <- [ 20 @@ deg, 40 @@ deg .. fullTurn ] ]
                    ps' = (_r *~ 0.6) <$> ps
                              
                scatterPlot ps $ key "points"
                scatterPlot ps' $ key "points"

                legendPlacement .= rightTop
                rLabel .= "r-axis"
                thetaLabel .= "theta-axis"