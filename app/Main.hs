{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG(loadImageSVG)
import System.IO.Unsafe

import qualified LimitExample

myCircle :: Diagram B
myCircle = circle 1

letterF' = polygon ( with
   & polyType .~ PolySides
   [cw, ccw, ccw, cw, cw, ccw, ccw, ccw, ccw]
   [2,1,1,1, 1, 2,  1,   3, 5]
   )
   where
     ccw = 90 @@ deg
     cw  = -90 @@ deg

letterF = text "F" <> square 1 # lw none

almostClosed :: Trail' Line V2 Double
almostClosed = fromVertices $ (map p2
  [( 0.30, 0.20), 
  ( 0.40, 0.20),
  ( 0.40, 0.45),
  ( 0.60, 0.45),
  ( 0.60, 0.55),
  ( 0.40, 0.55),
  ( 0.40, 0.70),
  ( 0.70, 0.70),
  ( 0.70, 0.80),
  ( 0.30, 0.80)])

frotated1 = rotateAround  (p2 (0.5, 0.5)) (45 @@ deg) letterF

closedLine  = pad 1.1 . center  . hsep 1
  $ [ --almostClosed # strokeLine
     almostClosed # closeLine # wrapLoop # strokeTrail --strokeLoop
    ]

-- 'escher' functions

turn' = rotate (90 @@ deg)
flip' = reflectX
toss  =   ( translate (0.5 * unitY))  . scale (1.0 / sqrt 2) . rotate (45 @@ deg)
-- scale (1.0 / sqrt 2) . rotateAround( p2 (0.0, 0.5)) (45 @@ deg)
-- above (beside (turn (turn (flip p))) (turn (turn p))) (beside (flip p) p)
foo = ( (((turn' . turn' . flip') theFish') ||| ((turn' . turn') theFish')) # centerXY # showOrigin )
       ===
       (((flip' theFish') ||| theFish') # centerXY # showOrigin )

theFish = unsafePerformIO $ loadImageSVG "fish2.png"

-- FIXME: translate! moveOriginBy v === translate (negated v).
-- position relative to unit square 
-- (unitSquare  # fc orange ) <> 
theFish' = --(unitSquare # dashingN [0.02] 0 ) <> 
           (( moveOriginBy (0.125 * unitX) (scaleToY 1 $ scaleToX 1.5 theFish )) 
              # rectEnvelope (p2 (-0.75,-0.5)) (r2 (1.25, 1.0)) {- -})  # showOrigin
             -- # clipBy (rect 1.25 1.0 # moveOriginBy (0.125 * unitX)) # showOrigin)
clipToUnit = rectEnvelope (p2 (-0.5,-0.5)) (r2 (1.0, 1.0))
twoFishes =  (( turn' . turn') theFish') <> ( theFish')

ttile f =    (f <> (fishN <> fishE) )
  where
    fishN = f # toss # flip'
    fishE = fishN # turn' # turn' # turn' 

utile f =    ( (fishN <> fishW) <> (fishE <> fishS)) 
  where

   fishN = f # toss # flip'
   fishW = fishN # turn'
   fishS = fishW # turn'
   fishE = fishS # turn'
   
-- henderson => h e n 
--              d e r
--              s o n
-- scale 1/6?
nonet p q r s t u v w x = scale (1/3) $ position $ zip 
                          ( p2 <$> [(-1, 1), (0, 1), (1, 1),
                                    (-1, 0), (0, 0), (1, 0),
                                    (-1, -1), (0, -1), (1, -1)
                                   ]) 
                          [p, q, r, s, t, u, v, w, x]
-- nonet p q r s t u v w x = ( p `beside'` q `beside'` r ) 
--                               `above'`
--                           ( s `beside'` t `beside'` u ) 
--                               `above'`
--                           ( v `beside'` w `beside'` x )       
-- scaleY 0.5 $  position $ zip (map p2 [(0, -0.5), (0, 0.5)]) [b, a]

--how to position to bounding box? (p ||| q)  === (r ||| s)
quartet p q r s = (p `beside'` q) `above'` (r `beside'` s) 
---centerXY $ position $ zip (map p2 [(0, 0.875), (1, 0.875), (0, 0), (1, 0)]) [p,q,r,s]

-- this is not the way, brother
--position $ zip (map p2 [(0.25, 0.75), (0.75, 0.75), (0.25, 0.25), (0.75, 0.25)]) [p,q,r,s]

side n p = quartet  s s (t # turn') t
  where
   s = if n == 1 then blank else side (n - 1) p
   t = ttile p
  
corner n p = quartet c s (s # turn') u
  where 
    (c,s) = if n == 1 then (blank, blank) 
             else (corner (n - 1) p, side (n - 1) p)
    u = utile p
  

blank = mempty

limit n fish = nonet  cornerNW sideN cornerNE 
                      sideW center sideE 
                      cornerSW sideS cornerSE
  where
    cornerNW = corner n fish
    cornerSW = turn' cornerNW
    cornerSE = turn' cornerSW
    cornerNE = turn' cornerSE
    sideN = side n fish  
    sideW = turn' sideN
    sideS = turn' sideW
    sideE = turn' sideS
    center = utile fish                           
-- is our atop or <>, beside, |||, ===
-- # showOrigin
-- stack run  -- --selection circle -w 100 -h 100 -o output.svg && firefox output.svg

above' a b = scaleY 0.5 $  position $ zip (map p2 [(0, -0.5), (0, 0.5)]) [b, a]
beside' a b = (scaleX 0.5 $  position $ zip (map p2 [(-0.5, 0), (0.5, 0)]) [a, b])
                   
nonetTest = unitSquare <>  (nonet
       (circle 1) (circle 1) (circle 1)
       (circle 1) (circle 1) (circle 1)
       (circle 1) (circle 1) (circle 1))
  where
    letter x = unitSquare <> (text x #scaleToX  (1.0) #scaleToY (1.0) #showOrigin )

-- wow, it's working! and I'm not an idiot. 
-- the only problem is that diagrams can't read svg files, it's not exactly my problem,
-- but how hard is to add svg support if most of the primitives already working, 
-- and actually are compiled into an svg by default??
-- TODO: outline !!

triangle' = (fromOffsets $ (map r2
  [(0.5, -0.5), (-1, 0), (0, 1), (0.5, -0.5)])) # strokeLine 

main :: IO ()
main = multiMain [
          ("circle", myCircle)
        , ("letterF", letterF)
        , ("closedLine", closedLine)
        , ("frotated1", foo)
        , ("twoFishes", twoFishes)
        , ("toss", unitSquare <> toss (unitSquare <> letterF))
        , ("justFish", theFish')
        , ("justTriangle",  (triangle') # showOrigin )
        , ("ttile", ttile theFish')
        , ("utile", square 1 <> utile theFish')
        , ("nonet", nonetTest)
        , ("side", side 2 (theFish'))
        , ("corner", corner 2 theFish')
        , ("above", square 1 <> above' theFish' theFish')
        , ("limit", limit 2 theFish')
        , ("example", LimitExample.example)
        , ("triangle", limit 3 $ triangle')
        ]


fishP = makeTile markingsP

makeTile :: [[P2 Double]] -> Diagram B
makeTile = showOrigin . lw thin . centerXY . mconcat . map fromVertices

markingsP = [
  [ (4^&4), (6^&0) ],
  [ (0^&3), (3^&4), (0^&8), (0^&3) ],
  [ (4^&5), (7^&6), (4^&10), (4^&5) ],
  [ (11^&0), (10^&4), (8^&8), (4^&13), (0^&16) ],
  [ (11^&0), (14^&2), (16^&2) ],
  [ (10^&4), (13^&5), (16^&4) ],
  [ (9^&6), (12^&7), (16^&6) ],
  [ (8^&8), (12^&9), (16^&8) ],
  [ (8^&12), (16^&10) ],
  [ (0^&16), (6^&15), (8^&16), (12^&12), (16^&12) ],
  [ (10^&16), (12^&14), (16^&13) ],
  [ (12^&16), (13^&15), (16^&14) ],
  [ (14^&16), (16^&15) ]
  ]