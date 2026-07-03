{-# LANGUAGE RecordWildCards #-}

{- | This is a module that provides a widget that displays ANSI text.
Neither Brick nor Graphics.Vty support ANSI text, so we have to
parse it ourselves.
-}
module Compat.Ansi (rawAnsi) where

import Brick (Widget, raw)
import Data.Bits (complement, (.&.), (.|.))
import Data.Char (isDigit)
import Data.Word (Word8)
import Graphics.Vty.Attributes qualified as A
import Graphics.Vty.Attributes.Color qualified as C
import Graphics.Vty.Image qualified as I
import Text.Read (readMaybe)

-- | A widget that displays ANSI text.
rawAnsi :: String -> Widget n
rawAnsi = raw . rowsToImage . parseAnsi
 where
  rowsToImage :: [[(St, String)]] -> I.Image
  rowsToImage rows =
    I.vertCat (map rowToImage rows)

  rowToImage :: [(St, String)] -> I.Image
  rowToImage [] =
    I.string (toAttr defSt) " "
  rowToImage segs =
    I.horizCat
      [ I.string (toAttr st) s
      | (st, s) <- segs
      , not (null s)
      ]

data St = St
  { stFg :: Maybe C.Color
  , stBg :: Maybe C.Color
  , stStyle :: A.Style
  }

defSt :: St
defSt =
  St
    { stFg = Nothing
    , stBg = Nothing
    , stStyle = A.defaultStyleMask
    }

toAttr :: St -> A.Attr
toAttr St{..} =
  A.Attr
    { A.attrStyle =
        if stStyle == A.defaultStyleMask
          then A.Default
          else A.SetTo stStyle
    , A.attrForeColor =
        maybe A.Default A.SetTo stFg
    , A.attrBackColor =
        maybe A.Default A.SetTo stBg
    , A.attrURL =
        A.Default
    }

parseAnsi :: String -> [[(St, String)]]
parseAnsi = finish . go defSt "" [] []
 where
  -- state, reversed-buffer, reversed-current-row, reversed-rows
  go ::
    St ->
    String ->
    [(St, String)] ->
    [[(St, String)]] ->
    String ->
    (St, String, [(St, String)], [[(St, String)]])
  go st buf row rows [] =
    (st, buf, row, rows)
  go st buf row rows ('\r' : xs) =
    go st buf row rows xs
  go st buf row rows ('\n' : xs) =
    let row' = flush st buf row
     in go st "" [] (reverse row' : rows) xs
  go st buf row rows ('\ESC' : '[' : xs) =
    let row' = flush st buf row
     in case takeCsi xs of
          Just (params, 'm', rest) ->
            let st' = applySgr st (parseParams params)
             in go st' "" row' rows rest
          Just (_, _, rest) ->
            -- Unsupported CSI escape: ignore it.
            go st "" row' rows rest
          Nothing ->
            -- Malformed escape at end: ignore it.
            go st "" row' rows []
  go st buf row rows (x : xs) =
    go st (x : buf) row rows xs

  finish (st, buf, row, rows) =
    reverse (reverse (flush st buf row) : rows)

  flush :: St -> String -> [(St, String)] -> [(St, String)]
  flush _ "" row =
    row
  flush st buf row =
    (st, reverse buf) : row

takeCsi :: String -> Maybe (String, Char, String)
takeCsi xs =
  case span isCsiParam xs of
    (_, []) -> Nothing
    (params, c : r) -> Just (params, c, r)
 where
  isCsiParam c =
    isDigit c || c == ';' || c == ':' || c == '?' || c == '-'

parseParams :: String -> [Int]
parseParams "" = [0]
parseParams s = map readParam (splitParams s)
 where
  readParam "" = 0
  readParam x = maybe 0 id (readMaybe x)

splitParams :: String -> [String]
splitParams [] = [""]
splitParams s =
  case break (\c -> c == ';' || c == ':') s of
    (a, []) -> [a]
    (a, _ : rest) -> a : splitParams rest

applySgr :: St -> [Int] -> St
applySgr st [] = st
applySgr st (x : xs) =
  case x of
    0 -> applySgr defSt xs
    1 -> applySgr (addStyle A.bold st) xs
    2 -> applySgr (addStyle A.dim st) xs
    3 -> applySgr (addStyle A.italic st) xs
    4 -> applySgr (addStyle A.underline st) xs
    5 -> applySgr (addStyle A.blink st) xs
    7 -> applySgr (addStyle A.reverseVideo st) xs
    9 -> applySgr (addStyle A.strikethrough st) xs
    22 -> applySgr (delStyle A.bold . delStyle A.dim $ st) xs
    23 -> applySgr (delStyle A.italic st) xs
    24 -> applySgr (delStyle A.underline st) xs
    25 -> applySgr (delStyle A.blink st) xs
    27 -> applySgr (delStyle A.reverseVideo st) xs
    29 -> applySgr (delStyle A.strikethrough st) xs
    39 -> applySgr (st{stFg = Nothing}) xs
    49 -> applySgr (st{stBg = Nothing}) xs
    n
      | 30 <= n && n <= 37 ->
          applySgr (st{stFg = Just (ansiColor (n - 30))}) xs
    n
      | 40 <= n && n <= 47 ->
          applySgr (st{stBg = Just (ansiColor (n - 40))}) xs
    n
      | 90 <= n && n <= 97 ->
          applySgr (st{stFg = Just (ansiColor (8 + n - 90))}) xs
    n
      | 100 <= n && n <= 107 ->
          applySgr (st{stBg = Just (ansiColor (8 + n - 100))}) xs
    -- foreground extended color:
    --   ESC[38;5;Nm
    --   ESC[38;2;R;G;Bm
    38 ->
      case xs of
        5 : n : rest ->
          applySgr (st{stFg = Just (ansi256 n)}) rest
        2 : r : g : b : rest ->
          applySgr (st{stFg = Just (rgb r g b)}) rest
        _ ->
          applySgr st xs
    -- background extended color:
    --   ESC[48;5;Nm
    --   ESC[48;2;R;G;Bm
    48 ->
      case xs of
        5 : n : rest ->
          applySgr (st{stBg = Just (ansi256 n)}) rest
        2 : r : g : b : rest ->
          applySgr (st{stBg = Just (rgb r g b)}) rest
        _ ->
          applySgr st xs
    _ ->
      applySgr st xs

addStyle :: A.Style -> St -> St
addStyle style st =
  st{stStyle = stStyle st .|. style}

delStyle :: A.Style -> St -> St
delStyle style st =
  st{stStyle = stStyle st .&. complement style}

ansiColor :: Int -> C.Color
ansiColor n =
  C.ISOColor (w8 n)

ansi256 :: Int -> C.Color
ansi256 n
  | n <= 15 = C.ISOColor (w8 n)
  | n <= 255 = C.Color240 (w8 (n - 16))
  | otherwise = C.Color240 239

rgb :: Int -> Int -> Int -> C.Color
rgb r g b =
  C.RGBColor (w8 r) (w8 g) (w8 b)

w8 :: Int -> Word8
w8 =
  fromIntegral . max 0 . min 255