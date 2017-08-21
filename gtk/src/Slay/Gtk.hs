{-# LANGUAGE OverloadedLabels #-}
module Slay.Gtk
  ( example
  ) where

import Control.Monad.IO.Class

import Data.Fixed
import Numeric.Natural
import Data.IORef
import Data.Word
import Data.Semigroup
import Data.List.NonEmpty as NonEmpty
import Lens.Micro.Platform
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function
import Data.Ratio

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Core
import Slay.Prim
import Slay.Cairo


data PreMatrix = PreMatrix
  { pmScale :: Centi,
    pmRotate :: Integer, -- 1/12
    pmOffset :: (Integer, Integer)
  }

makeLensesFor
  [ ("pmScale", "pmScaleL"),
    ("pmRotate", "pmRotateL"),
    ("pmOffset", "pmOffsetL") ]
  ''PreMatrix

prepareMatrix1 :: PreMatrix -> Cairo.Matrix
prepareMatrix1 PreMatrix{..} =
  Matrix.rotate (pi * realToFrac pmRotate / 12) $
  Matrix.scale (realToFrac pmScale) (realToFrac pmScale) $
  Matrix.identity

prepareMatrix2 :: PreMatrix -> Cairo.Matrix -> Cairo.Matrix
prepareMatrix2 PreMatrix{..} =
  Matrix.translate (realToFrac $ fst pmOffset) (realToFrac $ snd pmOffset)

data WithPhase x =
  PhaseConst x |
  PhaseCursor (Natural -> Bool -> x) |
  PhaseColor (Word8 -> x) |
  PhaseCurvature (Rational -> x)

withPhase :: Natural -> Bool -> Word8 -> Rational -> WithPhase x -> x
withPhase cursor cursorPhase colorPhase curvaturePhase = \case
  PhaseConst x -> x
  PhaseCursor mkX -> mkX cursor cursorPhase
  PhaseColor mkX -> mkX colorPhase
  PhaseCurvature mkX -> mkX curvaturePhase

type CollageElements = NonEmpty (Offset, SomeRenderElement WithPhase)

-- invariant: collageElements = mkElements label
data AppState = AppState
  { appStateLabel :: Text,
    appStateCursor :: Natural,
    appStatePreMatrix :: PreMatrix,
    appStateCollageElements :: ((CollageElements, Extents), Word8 -> Color)
  }

appStateMatrix :: AppState -> Cairo.Matrix
appStateMatrix = prepareMatrix1 . appStatePreMatrix

makeLensesFor [ ("appStateCursor","appStateCursorL") ] ''AppState

example :: IO ()
example = do
  _ <- Gtk.initGUI
  win <- Gtk.windowNew
  _ <- Gtk.on win Gtk.objectDestroy Gtk.mainQuit
  drawArea <- Gtk.drawingAreaNew
  Gtk.set drawArea
    [ Gtk.widgetExpand   Gtk.:= True
    , Gtk.widgetCanFocus Gtk.:= True
    , Gtk.widgetHasFocus Gtk.:= True
    ]
  Gtk.widgetAddEvents drawArea
    [ Gtk.PointerMotionMask
    , Gtk.ButtonPressMask
    , Gtk.ScrollMask
    ]
  let
    mkElements matrix label =
      case layoutElements (withExtents matrix) exampleLayout of
        Vis (mkElements', background) -> (mkElements' label, background)
  appStateRef <- newIORef $ fix $ \this ->
    AppState
      { appStateLabel = "Source",
        appStateCursor = 0,
        appStatePreMatrix = PreMatrix 1 0 (0, 0),
        appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this) }
  cursorPhaser <- createPhaser
  colorPhaser <- createPhaser
  _ <- flip Gtk.timeoutAdd 5 $ do
    Gtk.postGUIAsync (Gtk.widgetQueueDraw drawArea)
    updatePhaser cursorPhaser
    updatePhaser colorPhaser
    return True
  _ <- Gtk.on drawArea Gtk.draw $ do
    cursorPhase <- liftIO $ readPhaser cursorPhaser $ \w -> even (w `div` 100)
    colorPhase <- liftIO $ readPhaser colorPhaser $ \w -> case divMod w 256 of
      (d, m) -> fromIntegral $ if even d then m else 255 - m
    let curvaturePhase = (toInteger colorPhase % 255) * 2 - 1
    appState <- liftIO $ readIORef appStateRef
    let
      preMatrix = appStatePreMatrix appState
      matrix = prepareMatrix1 preMatrix
      ((elements, vextents), background) = appStateCollageElements appState
    viewport' <- setBackground (background colorPhase)
    let
      (vl, vr, vt, vb) = boundingBox $
        Matrix.transformPoint matrix (0, 0) :|
        Matrix.transformPoint matrix (r, 0) :
        Matrix.transformPoint matrix (0, b) :
        Matrix.transformPoint matrix (r, b) : []
        where
          Extents (fromIntegral -> r) (fromIntegral -> b) = vextents
      (w, h) = (vr - vl, vb - vt)
      (realToFrac -> ofs_l, _) =
        getExcess (floor $ fst viewport' :: Integer) (ceiling w)
      (realToFrac -> ofs_t, _) =
        getExcess (floor $ snd viewport' :: Integer) (ceiling h)
    Cairo.setMatrix (Matrix.translate (ofs_l - vl) (ofs_t - vt) (prepareMatrix2 preMatrix matrix))
    renderElements (withPhase (appStateCursor appState) cursorPhase colorPhase curvaturePhase) elements
  _ <- Gtk.on drawArea Gtk.keyPressEvent $ do
    keyVal <- Gtk.eventKeyVal
    label <- liftIO $ appStateLabel <$> readIORef appStateRef
    let
      moveCursorLeft = \case
        0 -> 0
        c -> c - 1
      moveCursorRight c
        | fromIntegral c >= Text.length label = c
        | otherwise = c + 1
    liftIO $ case keyVal of
      ArrowLeft -> do
        atomicModifyIORef' appStateRef ((,()) . over appStateCursorL moveCursorLeft)
        resetPhaser cursorPhaser
        return True
      ArrowRight -> do
        atomicModifyIORef' appStateRef ((,()) . over appStateCursorL moveCursorRight)
        resetPhaser cursorPhaser
        return True
      Delete -> do
        atomicModifyIORef' appStateRef $ \appState ->
          let
            cursor = appStateCursor appState
            (pre, post) = Text.splitAt (fromIntegral cursor) (appStateLabel appState)
            appState' = fix $ \this -> appState
              { appStateLabel = pre <> Text.drop 1 post,
                appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
              }
          in
            (appState', ())
        resetPhaser cursorPhaser
        return True
      Backspace -> do
        atomicModifyIORef' appStateRef $ \appState ->
          let
            cursor = appStateCursor appState
            (pre, post) = Text.splitAt (fromIntegral cursor) (appStateLabel appState)
            lbl' = Text.dropEnd 1 pre <> post
            appState' = fix $ \this -> appState
              { appStateLabel = lbl',
                appStateCursor = if Text.null pre then cursor else cursor - 1,
                appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
              }
          in
            (appState', ())
        resetPhaser cursorPhaser
        return True
      (Gtk.keyToChar -> Just c) -> do
        atomicModifyIORef' appStateRef $ \appState ->
          let
            cursor = appStateCursor appState
            (pre, post) = Text.splitAt (fromIntegral cursor) (appStateLabel appState)
            lbl' = pre <> Text.cons c post
            appState' = fix $ \this -> appState
              { appStateLabel = lbl',
                appStateCursor = cursor + 1,
                appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
              }
          in
            (appState', ())
        resetPhaser cursorPhaser
        return True

      _ -> do
        print keyVal
        return False
  _ <- Gtk.on drawArea Gtk.buttonPressEvent $ do
    btn <- Gtk.eventButton
    mods <- Gtk.eventModifier
    case btn of
      Gtk.MiddleButton -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = appStatePreMatrix appState
            preMatrix' = preMatrix & if Gtk.Control `elem` mods
              then pmScaleL .~ 1
              else pmOffsetL .~ (0, 0)
            appState' = fix $ \this -> appState
              { appStatePreMatrix = preMatrix',
                appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
              }
          in
            (appState', ())
        return True
      _ -> do
        liftIO $ print btn
        return False
  _ <- Gtk.on drawArea Gtk.scrollEvent $ do
    dir <- Gtk.eventScrollDirection
    mods <- Gtk.eventModifier
    case dir of
      Gtk.ScrollUp -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = preMatrix & pmScaleL %~ (+0.15),
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = preMatrix & pmOffsetL . _2 %~ (+5)
                }
          in
            (appState', ())
        return True
      Gtk.ScrollDown -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = preMatrix & pmScaleL %~ subtract 0.15,
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = preMatrix & pmOffsetL . _2 %~ subtract 5
                }
          in
            (appState', ())
        return True
      Gtk.ScrollLeft -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = preMatrix & pmRotateL %~ pred,
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = preMatrix & pmOffsetL . _1 %~ (+5)
                }
          in
            (appState', ())
        return True
      Gtk.ScrollRight -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = preMatrix & pmRotateL %~ succ,
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = preMatrix & pmOffsetL . _1 %~ subtract 5
                }
          in
            (appState', ())
        return True
      _ -> do
        liftIO $ print dir
        return False

  Gtk.containerAdd win drawArea
  Gtk.windowMaximize win
  Gtk.widgetShowAll win
  Gtk.mainGUI

pattern ArrowLeft :: Word32
pattern ArrowLeft = 65361

pattern ArrowRight :: Word32
pattern ArrowRight = 65363

--pattern ArrowUp = 65362
--pattern ArrowDown = 65364

pattern Delete :: Word32
pattern Delete = 65535

pattern Backspace :: Word32
pattern Backspace = 65288

newtype Phaser = Phaser (IORef Word)

createPhaser :: IO Phaser
createPhaser = Phaser <$> newIORef 0

resetPhaser :: Phaser -> IO ()
resetPhaser (Phaser r) = writeIORef r 0

updatePhaser :: Phaser -> IO ()
updatePhaser (Phaser r) = atomicModifyIORef' r (\x -> (x + 1, ()))

readPhaser :: Phaser -> (Word -> r) -> IO r
readPhaser (Phaser r) cont = cont <$> readIORef r

setBackground :: Color -> Cairo.Render (Double, Double)
setBackground background = do
  (x1, y1, x2, y2) <- Cairo.clipExtents
  let viewport@(w, h) = (x2 - x1, y2 - y1)
  viewport <$ do
    Cairo.rectangle 0 0 (realToFrac w) (realToFrac h)
    setSourceColor background
    Cairo.fill

data El = ElRect (PrimRect WithPhase) | ElText (PrimText WithPhase) | ElCurve (PrimCurve WithPhase) | ElCircle (PrimCircle WithPhase)

withExtents :: Cairo.Matrix -> El -> (Extents, SomeRenderElement WithPhase)
withExtents matrix = \case
  ElRect primRect -> (rectExtents primRect, SomeRenderElement primRect)
  ElText primText ->
    let pangoText = primTextPango matrix primText
    in (ptextExtents pangoText, SomeRenderElement pangoText)
  ElCurve primCurve -> (curveExtents primCurve, SomeRenderElement primCurve)
  ElCircle circle -> (makeCircleExtents circle, SomeRenderElement circle)

ubuntuFont :: Centi -> Font WithPhase
ubuntuFont size = Font "Ubuntu" size (PhaseConst (RGB 0 0 0)) FontWeightNormal

newtype Vis a = Vis (Text -> (a, Extents), Word8 -> Color)
  deriving (Functor)

exampleLayout :: Layout Vis El
exampleLayout = mkLayout $ Vis $
  let
    background colorPhase = RGB
      (colorPhase `div` 10)
      (colorPhase `div` 10)
      (colorPhase `div` 10)
    mkMsgbox msg =
      substrate (LRTB 5 5 5 5) (rect $ PhaseColor $ \colorPhase -> rgb colorPhase 130 200) $
      substrate (LRTB 1 1 1 1) (rect $ PhaseConst $ rgb 0 0 0) $
      substrate (LRTB 3 3 3 3) (rect $ PhaseConst $ rgb 255 255 255) $
      substrate (LRTB 3 3 3 3) (curve (PhaseCurvature Curvature) (PhaseColor $ \colorPhase -> rgb colorPhase 130 200) (PhaseConst (Direction True False)) ) $
      text (ubuntuFont 12) msg
        (PhaseCursor $ \cursor c -> if c then Just cursor else Nothing)
      -- substrate (LRTB 3 3 3 3) makeCircle
    msgboxWithExtents msg =
      let msgbox = mkMsgbox msg
      in (msgbox, collageExtents msgbox)
  in (msgboxWithExtents, background)

-- makeCircle :: PrimCircle a
-- makeCircle  = circle (Just $ RGB 200 200 200) (20 :: Double) (20 :: Double) (20 :: Double) (20 :: Double) (20 :: Double)

makeCircleExtents :: PrimCircle g -> Extents
makeCircleExtents crcl = Extents  (2 * (fromInteger . round $ circleRadius crcl)) (2 * (fromInteger . round $ circleRadius crcl))

getExcess :: Integral n => n -> n -> (n, n)
getExcess vacant actual
  | vacant > actual =
    let
      excess = max 0 (vacant - actual)
      excess1 = excess `quot` 2
      excess2 = excess - excess1
    in (excess1, excess2)
  | otherwise = (0, 0)

boundingBox :: Ord a => NonEmpty (a, a) -> (a, a, a, a)
boundingBox xs = (l,r,t,b)
  where
    ((Min l, Min t), (Max r, Max b)) =
      sconcat $ (\(x, y) -> ((Min x, Min y), (Max x, Max y))) <$> xs

instance Inj (PrimRect WithPhase) El where
  inj = ElRect

instance Inj (PrimText WithPhase) El where
  inj = ElText

instance Inj (PrimCurve WithPhase) El where
  inj = ElCurve

instance Inj (PrimCircle WithPhase) El where
  inj = ElCircle
