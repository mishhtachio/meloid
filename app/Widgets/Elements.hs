module Widgets.Elements (
  ElementScaffoldName (..),
  ElementName (..),
  HeaderName (..),
  CollapsingSwitch (..),
  ElementPath,
  displayElementName,
) where

import Brick
import Brick qualified as W
import Brick.Widgets.Border qualified as Bd
import Brick.Widgets.Center qualified as C
import Data.Bool (bool)
import Data.List
import Data.Map qualified as Map
import Data.Vector qualified as Vec
import Lens.Micro
import Lens.Micro.Mtl
import Types
import Widgets.Common (strClippedWithEllipsis)
import Widgets.Controls
import Widgets.Lists
import Widgets.Visual.EQ

{- | The representation of layout elements in the edit mode.
They appear as scaffolds in the edit mode which supports
moving, resizing, deleting, and adding new elements.
-}
data ElementScaffoldName = ElementScaffoldName ElementPath
  deriving (Show, Eq)

data ElementName = ElementName ElementPath
  deriving (Show, Eq)

data HeaderName = HeaderName ElementPath
  deriving (Show, Eq)

data CollapsingSwitch = CollapsingSwitch ElementPath
  deriving (Show, Eq)

-- | The location of an element in the layout tree.
type ElementPath = [Int]

pathVariant :: ElementPath -> Int
pathVariant = foldl' (\acc i -> acc * 131 + i + 1) 0

displayElementName :: LayoutElement -> String
displayElementName (EHBox _ _) = ""
displayElementName (EVBox _ _) = ""
displayElementName (ETabs _) = ""
displayElementName EAlbumList = "ALBUMS"
displayElementName ETrackList = "TRACKS"
displayElementName ECurrentQueue = "QUEUE"
displayElementName EEqualizer = "EQUALIZER"
displayElementName EPlaceholder = "EMPTY"

instance Drawable St ElementName where
  draw (ElementName path) st
    | st ^. stMode == EditMode = drawNamed st (ElementScaffoldName path)
    | otherwise =
        case st ^. stLayoutElement path of
          Nothing -> W.emptyWidget
          Just element -> drawElement path element st
  parent (ElementName path) = case path of
    [] -> Just (ParentView MainView)
    is -> ParentName . mName . ElementName . fst <$> unsnoc is
  variant (ElementName path) = pathVariant path

-- | Draw all the elements of a layout element.
drawElement :: ElementPath -> LayoutElement -> St -> Widget (MName St)
drawElement path element st =
  go path element
 where
  go currentPath = \case
    EHBox weights children ->
      W.hBox $
        applyElementSpacing (W.padLeft (W.Pad 1)) children $
          zipWith W.hLimitPercent (layoutPercents weights children) $
            drawChildren children
    EVBox weights children ->
      W.vBox $
        applyElementSpacing (W.padTop (W.Pad 1)) children $
          zipWith W.vLimitPercent (layoutPercents weights children) $
            drawChildren children
    ETabs children ->
      drawTabs currentPath children
    EAlbumList ->
      drawLeaf currentPath st drawAllAlbumList
    ETrackList ->
      drawLeaf currentPath st drawTrackList
    ECurrentQueue ->
      drawLeaf currentPath st drawCurrentQueueList
    EEqualizer ->
      drawLeaf currentPath st drawEqualizerPanel
    EPlaceholder ->
      W.emptyWidget
   where
    drawChildren :: [LayoutElement] -> [Widget (MName St)]
    drawChildren children =
      drawNamed st . ElementName <$> childPaths children currentPath

    drawTabs :: ElementPath -> [LayoutElement] -> Widget (MName St)
    drawTabs _path _children = W.emptyWidget -- TODO
    drawLeaf p st' drawBody =
      W.vBox $
        [ drawNamed st' (HeaderName p)
        , bool W.emptyWidget (drawBody st') (isCollapsed p st')
        ]

    isCollapsed p st' = not $ st' ^. stIsTriggered (mName $ ElementName p)

  -- Calculate the percentage widths of each child element.
  -- It is always the last child that takes up the remaining space.
  layoutPercents :: Maybe [Double] -> [a] -> [Int]
  layoutPercents weights children =
    remainingPercents effectiveWeights
   where
    effectiveWeights =
      case weights of
        Just values
          | length values == length children
          , all (> 0) values ->
              values
        _ ->
          replicate (length children) 1

  remainingPercents :: [Double] -> [Int]
  remainingPercents = \case
    [] -> []
    [_] -> [100]
    value : rest ->
      let remaining = value + sum rest
          current = max 1 . floor $ (value / remaining) * 100
       in current : remainingPercents rest

  -- Scrollbar is considered as a one-size divider.
  -- So we don't need padding when the element has a scrollbar.
  hasScrollBar :: LayoutElement -> Bool
  hasScrollBar = \case
    EAlbumList -> True
    ETrackList -> True
    ECurrentQueue -> True
    EEqualizer -> True
    EPlaceholder -> False
    EHBox _ children -> any hasScrollBar children
    EVBox _ children -> any hasScrollBar children
    ETabs children -> any hasScrollBar children

  applyElementSpacing ::
    (Widget (MName St) -> Widget (MName St)) ->
    [LayoutElement] ->
    [Widget (MName St)] ->
    [Widget (MName St)]
  applyElementSpacing pad elements widgets =
    case zip elements widgets of
      [] -> []
      (firstElement, firstWidget) : rest ->
        firstWidget
          : [ applyPadding left right widget
            | ((left, _), (right, widget)) <- zip ((firstElement, firstWidget) : rest) rest
            ]
   where
    applyPadding left right
      | hasScrollBar left || hasScrollBar right = id
      | otherwise = pad

instance Drawable St ElementScaffoldName where
  draw (ElementScaffoldName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element -> drawElementScaffold path element st
  parent (ElementScaffoldName path) = case path of
    [] -> Just (ParentView MainView)
    is -> ParentName . mName . ElementScaffoldName . fst <$> unsnoc is
  variant (ElementScaffoldName path) = pathVariant path
  onMouseLeftUp (ElementScaffoldName path) =
    Just $ \_ ->
      use (stLayoutElement path) >>= \case
        Nothing ->
          logReqDebug "onMouseLeftUp" ("missing layout element at " <> show path)
        Just element ->
          logReqDebug
            "onMouseLeftUp"
            (formatElementName element <> " at " <> show path)

instance Drawable St HeaderName where
  draw (HeaderName path) st =
    case st ^. stLayoutElement path of
      Nothing -> W.emptyWidget
      Just element ->
        case drawHeader path st element of
          [] -> W.emptyWidget
          widgets ->
            W.vLimit 1 $
              W.hBox widgets
  parent (HeaderName path) = Just . ParentName . mName . ElementName $ path
  variant (HeaderName path) = pathVariant path

instance Drawable St CollapsingSwitch where
  draw (CollapsingSwitch path) st =
    W.withAttr (attrName "label") . W.str . (<> currentLabel) $
      bool " - " " + " $
        st ^. stIsTriggered (mName $ ElementName path)
   where
    currentLabel =
      case st ^. stLayoutElement path of
        Nothing -> ""
        Just currentElement -> displayElementName currentElement <> " "
  parent (CollapsingSwitch path) =
    Just . ParentName . mName . ElementName $ path
  variant (CollapsingSwitch path) = pathVariant path
  onMouseLeftUp (CollapsingSwitch path) = Just $ \_ ->
    use (stIsTriggered (mName $ ElementName path)) >>= \case
      True -> unTrigger (mName $ ElementName path)
      False -> trigger (mName $ ElementName path)

drawHeader :: ElementPath -> St -> LayoutElement -> [Widget (MName St)]
drawHeader path st = \case
  EAlbumList ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    ]
  ETrackList ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    , W.padLeft W.Max $
        W.withAttr (attrName "meta") $
          strClippedWithEllipsis album
    ]
  ECurrentQueue ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    , W.padLeft W.Max $ drawNamed st ShuffleButton
    , W.padLeft (W.Pad 1) $ drawNamed st ReverseOrderButton
    , W.padLeft (W.Pad 1) . W.padRight (W.Pad 1) $ drawNamed st ClearButton
    ]
  EEqualizer ->
    [ drawNamed st $ CollapsingSwitch path
    , W.fill ' '
    , W.padLeft W.Max $ drawNamed st EQSwitch
    ]
  _ -> []
 where
  selectedAlbum = (st ^. stSelectedAlbum) >>= ((st ^. stConfig . csAllAlbums) Vec.!?)
  album = maybe "" albumName selectedAlbum

-- | Draw all the scaffolding of a layout element.
drawElementScaffold :: ElementPath -> LayoutElement -> St -> Widget (MName St)
drawElementScaffold path element st =
  case element of
    EHBox weights children ->
      drawElementContainer (formatElementName element) $
        W.hBox $
          applyPlaceholderSpacing (W.padLeft (W.Pad 1)) $
            zipWith W.hLimitPercent (layoutPercents weights children) $
              drawChildren children
    EVBox weights children ->
      drawElementContainer (formatElementName element) $
        W.vBox $
          applyPlaceholderSpacing (W.padTop (W.Pad 1)) $
            zipWith W.vLimitPercent (layoutPercents weights children) $
              drawChildren children
    ETabs children ->
      drawElementContainer (formatElementName element) $
        W.vBox $
          applyPlaceholderSpacing (W.padTop (W.Pad 1)) $
            drawChildren children
    leaf ->
      drawElementPlaceholder (formatElementName leaf)
 where
  drawChildren :: [LayoutElement] -> [Widget (MName St)]
  drawChildren children =
    drawNamed st . ElementScaffoldName <$> childPaths children path

  layoutPercents :: Maybe [Double] -> [a] -> [Int]
  layoutPercents weights children =
    remainingPercents effectiveWeights
   where
    effectiveWeights =
      case weights of
        Just values
          | length values == length children
          , all (> 0) values ->
              values
        _ ->
          replicate (length children) 1

  drawElementPlaceholder label =
    Bd.border $ C.center $ W.str label

  drawElementContainer label =
    Bd.borderWithLabel (W.str (" " <> label <> " "))

  remainingPercents :: [Double] -> [Int]
  remainingPercents = \case
    [] -> []
    [_] -> [100]
    value : rest ->
      let remaining = value + sum rest
          current = max 1 . floor $ (value / remaining) * 100
       in current : remainingPercents rest

  applyPlaceholderSpacing _ [] = []
  applyPlaceholderSpacing pad (widget : widgets) =
    widget : fmap pad widgets

childPaths :: [LayoutElement] -> ElementPath -> [ElementPath]
childPaths children parent' =
  fmap (\i -> parent' <> [i]) [0 .. length children - 1]

drawAllAlbumList :: St -> Widget (MName St)
drawAllAlbumList st = drawNamed st AllAlbumList

drawTrackList :: St -> Widget (MName St)
drawTrackList st = drawNamed st TrackList

drawCurrentQueueList :: St -> Widget (MName St)
drawCurrentQueueList st = drawNamed st QueueSongList

drawEqualizerPanel :: St -> Widget (MName St)
drawEqualizerPanel st
  | Map.null (st ^. stConfig . csEQConfigs) = W.emptyWidget
drawEqualizerPanel st =
  W.hBox
    [ W.hLimit 11 $ drawNamed st EQConfigList
    , case st ^. stIsTriggered (mName EQSwitch) of
        False -> drawNamed st EQCurveVisualizer
        True -> W.padLeft (W.Pad 1) $ drawNamed st EQGainBarsViewport
    ]
