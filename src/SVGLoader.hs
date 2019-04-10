module SVGLoader where

import qualified Graphics.Svg as Svg
import System.IO.Unsafe

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import qualified Graphics.Svg.Types as Svg.Types

svgFish = unsafePerformIO $ maybe (error "no fish") id <$> (Svg.loadSvgFile "fish.svg")

emptyDiag = (lw none $ square 16)

fishToDiagram :: Svg.Document -> Diagram B
fishToDiagram doc = foldl' (\r a -> r <> drawElement a) emptyDiag (Svg.Types._elements doc)

drawElement :: Svg.Tree -> Diagram B 
drawElement (Svg.Types.PathTree (Svg.Path attrs definitions)) = drawSVGDefinitions definitions
drawElement (Svg.Types.GroupTree (Svg.Group attrs children viewBox aspectRatio)) = 
             foldl' (\r a -> r <> drawElement a) emptyDiag children
drawElement e = error $ "can't handle " <> (show e) <> "(yet)"

-- renderSVG "meout.svg" (dims (r2 (800.0, 800.0))) (fishToDiagram svgFish)
-- reverse engineer Diagram => SVG => Diagram?
-- need to fold over the path
drawSVGDefinitions :: [Svg.Types.PathCommand] -> Diagram B
drawSVGDefinitions xs = emptyDiag


{-

MoveTo !Origin ![RPoint]	

M or m command
LineTo !Origin ![RPoint]	

Line to, L or l Svg path command.
HorizontalTo !Origin ![Coord]	
Equivalent to the H or h svg path command.

VerticalTo !Origin ![Coord]	
Equivalent to the V or v svg path command.

CurveTo !Origin ![(RPoint, RPoint, RPoint)]	
Cubic bezier, C or c command

SmoothCurveTo !Origin ![(RPoint, RPoint)]	
Smooth cubic bezier, equivalent to S or s command

QuadraticBezier !Origin ![(RPoint, RPoint)]	
Quadratic bezier, Q or q command

SmoothQuadraticBezierCurveTo !Origin ![RPoint]	
Quadratic bezier, T or t command

EllipticalArc !Origin ![(Coord, Coord, Coord, Bool, Bool, RPoint)]	
Eliptical arc, A or a command.

EndPath	 -- Close the path, Z or z svg path command.

-}
