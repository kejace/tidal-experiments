{-# LANGUAGE NoMonomorphismRestriction #-}

module Vis where

import qualified Graphics.Rendering.Cairo as C 
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB
import Control.Applicative
import Sound.Tidal.Parse
import Sound.Tidal.Pattern
import Sound.Tidal.Utils

import Data.Monoid

import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as D

cPDF :: FilePath -> Pattern (Colour Double) -> IO ()
cPDF fn pat = D.renderSVG fn (D.Width 800) $ D.reflectY $ (mconcat circles) where 
   circles :: [D.Diagram D.SVG D.R2]           
   circles = map dRenderEventPure (map renderEventPure (events pat))

dRenderEvent :: (t, (Rational, Rational), [Colour Double]) -> D.Diagram D.SVG D.R2
dRenderEvent (_, (s,e), cs) = D.vcat (zipWith (D.fc) cs (repeat (D.circle 1)))

dRenderEventPure :: ([(Double, Double, Double, Double)], [Colour Double]) -> D.Diagram D.SVG D.R2 
dRenderEventPure (cord, col) = mconcat $ zipWith (D.fc) col (map toDim cord) where 
  toDim :: (Double, Double, Double, Double) -> D.Diagram D.SVG D.R2
  toDim (x, y, w, h) = D.translate (D.mkR2 x y) (D.rect w h)
                 
renderEventPure (_, (s,e), (cs)) = (drawBlocks cs 0, cs)
                                  
   where height = 1/(fromIntegral $ length cs)
         drawBlocks [] _ = []
         drawBlocks (c:cs) n = (x, y, (w + 0.1), h):drawBlocks cs (n+1)
           where x = (fromRational s)
                 y = (fromIntegral n) * height
                 w = (fromRational (e-s))
                 h = height

events pat = (map (mapSnd' (\(s,e) -> ((s - (ticks/2))/speed,(e - (ticks/2))/speed))) $ arc (segment pat) ((ticks/2), (ticks/2)+speed))
  where speed = 1
ticks = 0
pat = "[red blue green,orange purple]"
