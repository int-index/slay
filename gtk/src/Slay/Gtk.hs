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
import Data.Foldable
import Data.List.NonEmpty as NonEmpty
import Lens.Micro.Platform
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified Graphics.Rendering.Cairo as Cairo

import Slay.Cairo
import Slay.Gtk.PreMatrix
import Slay.Gtk.Phaser
import Slay.Gtk.KeyCode
import Slay.Gtk.Util

data WithPhase x =
  PhaseConst x |
  PhaseCursor (Natural -> Bool -> x) |
  PhaseColor (Word8 -> x) |
  PhaseCurvature (Rational -> x) |
  PhaseWidth (Unsigned -> x)

withPhase :: Natural -> Bool -> Word8 -> Rational -> Unsigned -> WithPhase x -> x
withPhase cursor cursorPhase colorPhase curvaturePhase widthPhase = \case
  PhaseConst x -> x
  PhaseCursor mkX -> mkX cursor cursorPhase
  PhaseColor mkX -> mkX colorPhase
  PhaseCurvature mkX -> mkX curvaturePhase
  PhaseWidth mkX -> mkX widthPhase

type CollageElements = NonEmpty (Offset, SomeRenderElement WithPhase)

type CollageElements' = ((CollageElements, Extents), Word8 -> Color)

-- invariant: collageElements = mkElements label
data AppState = AppState
  { appStateLabel :: Text,
    appStateCursor :: Natural,
    appStatePreMatrix :: CachedPreMatrix,
    appStateCollageElements :: CollageElements'
  }

appStateMatrix :: AppState -> Cairo.Matrix
appStateMatrix = cpmMatrix1 . appStatePreMatrix

appStateCursorL :: Lens' AppState Natural
appStateCursorL = lens appStateCursor (\app x -> app { appStateCursor = x })

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
    mkElements :: Cairo.Matrix -> Text -> CollageElements'
    mkElements matrix label =
      case layoutElements (withExtents matrix) exampleLayout of
        Vis (mkElements', background) -> (mkElements' label, background)
  appStateRef <- newIORef $ fix $ \this ->
    AppState
      { appStateLabel = "Source",
        appStateCursor = 0,
        appStatePreMatrix = cachedPreMatrix $ PreMatrix 1 0 (0, 0),
        appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this) }
  cursorPhaser <- createPhaser
  colorPhaser <- createPhaser
  widthPhaser <- createPhaser
  curvaturePhaser <- createPhaser
  _ <- flip Gtk.timeoutAdd 5 $ do
    Gtk.postGUIAsync (Gtk.widgetQueueDraw drawArea)
    traverse_ updatePhaser [cursorPhaser, colorPhaser, widthPhaser, curvaturePhaser]
    return True
  _ <- Gtk.on drawArea Gtk.draw $ do
    cursorPhase <- liftIO $ readPhaser cursorPhaser $ \w -> even (w `div` 100)
    colorPhase <- liftIO $ readPhaser colorPhaser $ \w -> case divMod w 256 of
      (d, m) -> fromIntegral $ if even d then m else 255 - m
    widthPhase <- liftIO $ readPhaser widthPhaser $ \w -> case divMod w 1256 of
      (d, m) -> fromIntegral $ if even d then m else 1255 - m
    curvaturePhase <- liftIO $ readPhaser curvaturePhaser $ \w -> case divMod w 1256 of
      (d, m) -> fromIntegral $ if even d then m else 1255 - m
    appState <- liftIO $ readIORef appStateRef
    let
      CachedPreMatrix _ matrix matrix' = appStatePreMatrix appState
      ((elements, vextents), background) = appStateCollageElements appState
    viewport' <- setBackground (background colorPhase)
    let
      (vl, vr, vt, vb) = boundingBox $
        Matrix.transformPoint matrix (0, 0) :|
        Matrix.transformPoint matrix (r, 0) :
        Matrix.transformPoint matrix (0, b) :
        Matrix.transformPoint matrix (r, b) : []
        where
          Extents (toSigned -> r) (toSigned -> b) = vextents
      (w, h) = (vr - vl, vb - vt)
      ofs_l = snap $ getExcess (fst viewport') w / 2
      ofs_t = snap $ getExcess (snd viewport') h / 2
    Cairo.setMatrix (Matrix.translate (ofs_l - vl) (ofs_t - vt) matrix')
    renderElements (withPhase (appStateCursor appState) cursorPhase colorPhase curvaturePhase widthPhase) elements
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
      K_Left -> do
        atomicModifyIORef' appStateRef ((,()) . over appStateCursorL moveCursorLeft)
        resetPhaser cursorPhaser
        return True
      K_Right -> do
        atomicModifyIORef' appStateRef ((,()) . over appStateCursorL moveCursorRight)
        resetPhaser cursorPhaser
        return True
      K_Delete -> do
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
      K_BackSpace -> do
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
            preMatrix = cpmPreMatrix $ appStatePreMatrix appState
            preMatrix' = preMatrix & if Gtk.Control `elem` mods
              then pmScaleL .~ 1
              else pmOffsetL .~ (0, 0)
            appState' = fix $ \this -> appState
              { appStatePreMatrix = cachedPreMatrix preMatrix',
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
            preMatrix = cpmPreMatrix $ appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmScaleL %~ (+0.15),
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmOffsetL . _2 %~ (+5)
                }
          in
            (appState', ())
        return True
      Gtk.ScrollDown -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = cpmPreMatrix $ appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmScaleL %~ subtract 0.15,
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmOffsetL . _2 %~ subtract 5
                }
          in
            (appState', ())
        return True
      Gtk.ScrollLeft -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = cpmPreMatrix $ appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmRotateL %~ pred,
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmOffsetL . _1 %~ (+5)
                }
          in
            (appState', ())
        return True
      Gtk.ScrollRight -> do
        liftIO $ atomicModifyIORef' appStateRef $ \appState ->
          let
            preMatrix = cpmPreMatrix $ appStatePreMatrix appState
            appState' = if Gtk.Control `elem` mods
              then fix $ \this -> appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmRotateL %~ succ,
                  appStateCollageElements = mkElements (appStateMatrix this) (appStateLabel this)
                }
              else appState
                { appStatePreMatrix = cachedPreMatrix $ preMatrix & pmOffsetL . _1 %~ subtract 5
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

data El = ElRect (PrimRect WithPhase) | ElText (PrimText WithPhase) | ElCurve (PrimCurve WithPhase) | ElCircle (PrimCircle WithPhase)

withExtents :: Cairo.Matrix -> El -> (Extents, SomeRenderElement WithPhase)
withExtents matrix = \case
  ElRect primRect -> (rectExtents primRect, SomeRenderElement primRect)
  ElText primText ->
    let pangoText = primTextPango matrix primText
    in (snapExtents $ ptextExtents pangoText, SomeRenderElement pangoText)
  ElCurve primCurve -> (curveExtents primCurve, SomeRenderElement primCurve)
  ElCircle circ -> (circleExtents circ, SomeRenderElement circ)

ubuntuFont :: Centi -> Font
ubuntuFont size = Font "Ubuntu" size FontWeightNormal

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
      substrate (LRTB 3 3 3 3) (curve
        (PhaseCurvature (Curvature.(subtract 1).(/628)))
        (PhaseColor $ \colorPhase -> rgb colorPhase 130 200)
        (PhaseConst (Direction True False))
        (PhaseWidth ((+1).(/1000)))
        (Just $ rgb 0 0 255)) $
      collageCompose (Offset 200 0)
        (substrate (LRTB 0 0 0 0) (rect $ PhaseConst $ rgb 255 0 0) $ collageSingleton makeCircle)
        (text (ubuntuFont 12) (PhaseConst $ rgb 0 0 0) msg
        (PhaseCursor $ \cursor c -> if c then Just cursor else Nothing))
    msgboxWithExtents msg =
      let msgbox = mkMsgbox msg
      in (msgbox, collageExtents msgbox)
  in (msgboxWithExtents, background)

makeCircle :: El
makeCircle = circle (PhaseConst $ rgb 205 255 215) 15

instance Inj (PrimRect WithPhase) El where
  inj = ElRect

instance Inj (PrimText WithPhase) El where
  inj = ElText

instance Inj (PrimCurve WithPhase) El where
  inj = ElCurve

instance Inj (PrimCircle WithPhase) El where
  inj = ElCircle
