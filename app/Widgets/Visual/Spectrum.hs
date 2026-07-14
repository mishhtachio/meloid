-- | This module renders the live PipeWire spectrum as dense braille bars.
module Widgets.Visual.Spectrum (
  SpectrumVisualizer (..),
) where

import Brick qualified as B
import Data.Bool (bool)
import Data.Vector qualified as Vec
import GHC.Bits ((.|.))
import Graphics.Vty.Attributes qualified as A
import Graphics.Vty.Image qualified as I
import Lens.Micro
import Types
import Utils (clampValue, formatFrequencyLabel)
import Widgets.Elements.Common (ElementNode (..), ElementPath, pathVariant)

data SpectrumVisualizer = SpectrumVisualizer ElementPath

data SpectrumPalette = SpectrumPalette
  { spectrumLowAttr :: A.Attr
  , spectrumAccentAttr :: A.Attr
  , spectrumPeakAttr :: A.Attr
  , spectrumAxisAttr :: A.Attr
  , spectrumLabelAttr :: A.Attr
  }

instance Drawable St SpectrumVisualizer where
  draw _ st = B.Widget B.Greedy B.Greedy $ do
    context <- B.getContext
    palette <- lookupSpectrumPalette
    B.render . B.raw $
      renderSpectrumImage
        palette
        (context ^. B.availWidthL)
        (context ^. B.availHeightL)
        (st ^. stSpectrum . ssLevels)
  parent (SpectrumVisualizer path) = Just . ParentName . mName $ ElementNode path
  variant (SpectrumVisualizer path) = pathVariant path

lookupSpectrumPalette :: B.RenderM n SpectrumPalette
lookupSpectrumPalette =
  SpectrumPalette
    <$> B.lookupAttrName (B.attrName "spectrumLow")
    <*> B.lookupAttrName (B.attrName "spectrumAccent")
    <*> B.lookupAttrName (B.attrName "spectrumPeak")
    <*> B.lookupAttrName (B.attrName "spectrumAxis")
    <*> B.lookupAttrName (B.attrName "spectrumLabel")

renderSpectrumImage :: SpectrumPalette -> Int -> Int -> Vec.Vector Double -> I.Image
renderSpectrumImage palette width height levels =
  if height > 1
    then I.vertCat [plot, renderAxis palette charWidth]
    else plot
 where
  charWidth = max 1 width
  plot = renderPlot palette charWidth (max 1 (height - 1)) levels

renderPlot :: SpectrumPalette -> Int -> Int -> Vec.Vector Double -> I.Image
renderPlot palette charWidth charHeight levels =
  I.vertCat
    [ I.horizCat [renderCell cellX cellY | cellX <- [0 .. charWidth - 1]]
    | cellY <- [0 .. charHeight - 1]
    ]
 where
  plotWidth = charWidth * 2
  plotHeight = charHeight * 4
  values = [levelAt x | x <- [0 .. plotWidth - 1]]

  levelAt x
    | Vec.null levels = -90
    | otherwise = Vec.maximum $ Vec.slice start (end - start + 1) levels
   where
    count = Vec.length levels
    start =
      min (count - 1) $
        floor @Double (fromIntegral x * fromIntegral count / fromIntegral plotWidth)
    end =
      min (count - 1) $
        max start (ceiling @Double (fromIntegral (x + 1) * fromIntegral count / fromIntegral plotWidth) - 1)

  dots value =
    clampValue 0 plotHeight . round @Double $
      (clampValue (-90) 0 value + 90) / 90 * fromIntegral plotHeight

  renderCell cellX cellY =
    case mask of
      0 -> I.string (spectrumAxisAttr palette) " "
      _ -> I.string (barAttr cellY) [toEnum (0x2800 + mask)]
   where
    leftDots = dots $ values !! (cellX * 2)
    rightDots = dots $ values !! (cellX * 2 + 1)
    mask =
      foldl'
        (.|.)
        0
        [ brailleBit localX localY
        | localX <- [0 .. 1]
        , localY <- [0 .. 3]
        , let subY = cellY * 4 + localY
        , let filled = case localX == 0 of
                True -> subY >= plotHeight - leftDots
                False -> subY >= plotHeight - rightDots
        , filled
        ]

  barAttr cellY
    | heightFromBottom * 5 >= plotHeight * 4 = spectrumPeakAttr palette
    | heightFromBottom * 5 >= plotHeight * 2 = spectrumAccentAttr palette
    | otherwise = spectrumLowAttr palette
   where
    heightFromBottom = plotHeight - cellY * 4

renderAxis :: SpectrumPalette -> Int -> I.Image
renderAxis palette width = I.horizCat [cell x | x <- [0 .. width - 1]]
 where
  markers =
    [ (markerX frequency, formatFrequencyLabel frequency)
    | frequency <- [30, 100, 400, 1000, 4000, 18000]
    ]
  labels = foldl' place [] markers

  markerX frequency =
    clampValue 0 (width - 1) . round @Double $
      log (frequency / 30) / log (18000 / 30) * fromIntegral (width - 1)

  place overlays (tick, text) =
    let start = clampValue 0 (max 0 $ width - length text) (tick - length text `div` 2)
     in bool ((start, text) : overlays) overlays $ any (overlaps start text) overlays

  overlaps start text (otherStart, otherText) =
    start < otherStart + length otherText && otherStart < start + length text

  cell x =
    case labelAt x labels of
      Just char -> I.string (spectrumLabelAttr palette) [char]
      Nothing
        | x `elem` map fst markers -> I.string (spectrumAccentAttr palette) "┴"
        | otherwise -> I.string (spectrumAxisAttr palette) "─"

  labelAt _ [] = Nothing
  labelAt x ((start, text) : rest)
    | x >= start && x < start + length text = Just $ text !! (x - start)
    | otherwise = labelAt x rest

brailleBit :: Int -> Int -> Int
brailleBit 0 0 = 0x01
brailleBit 0 1 = 0x02
brailleBit 0 2 = 0x04
brailleBit 0 3 = 0x40
brailleBit 1 0 = 0x08
brailleBit 1 1 = 0x10
brailleBit 1 2 = 0x20
brailleBit 1 3 = 0x80
brailleBit _ _ = 0
