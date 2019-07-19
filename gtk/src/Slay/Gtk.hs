{-# OPTIONS -Wno-type-defaults #-}
{-# LANGUAGE OverloadedLabels #-}

module Slay.Gtk
  ( example
  ) where

import Control.Monad.IO.Class

import Data.Fixed
import Numeric.NonNegative
import Numeric.Natural
import Data.IORef
import Data.Word
import Data.Foldable
import Data.List.NonEmpty as NonEmpty
import Lens.Micro.Platform
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Function
import Control.Applicative

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.Rendering.Cairo.Matrix as Matrix
import qualified Graphics.Rendering.Cairo as Cairo

import Inj
import Inj.Base ()

import Slay.Cairo
import Slay.Combinators
import Slay.Gtk.PreMatrix
import Slay.Gtk.Phaser
import Slay.Gtk.KeyCode
import Slay.Gtk.Util

data PhaseCtx =
  PhaseCtx
    { phCursor :: Natural,
      phCursorPhase :: Bool,
      phColorPhase :: Word8,
      phCurvaturePhase :: Rational,
      phWidthPhase :: NonNegative Double }

newtype WithPhase x = WithPhase (PhaseCtx -> x)
  deriving (Functor, Applicative)

instance Num a => Num (WithPhase a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

phaseConst :: x -> WithPhase x
phaseConst x = WithPhase (\_ -> x)

phaseCursor :: (Natural -> Bool -> x) -> WithPhase x
phaseCursor mkX = WithPhase (\PhaseCtx{..} -> mkX phCursor phCursorPhase)

phaseColor :: (Word8 -> x) -> WithPhase x
phaseColor mkX = WithPhase (\PhaseCtx{..} -> mkX phColorPhase)

phaseCurvature :: (Rational -> x) -> WithPhase x
phaseCurvature mkX = WithPhase (\PhaseCtx{..} -> mkX phCurvaturePhase)

phaseWidth :: (NonNegative Double -> x) -> WithPhase x
phaseWidth mkX = WithPhase (\PhaseCtx{..} -> mkX phWidthPhase)

instance Inj p x => Inj p (WithPhase x) where
  inj = pure . inj

type CollageRender = ((CairoRender WithPhase, Extents), Word8 -> Color)

-- invariant: appStateCollageRender = mkCollageRender label
data AppState = AppState
  { appStateLabel :: Text,
    appStateCursor :: Natural,
    appStatePreMatrix :: CachedPreMatrix,
    appStateCollageRender :: CollageRender
  }

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
    mkCollageRender :: Text -> CollageRender
    mkCollageRender label =
      let
        (mkCollage, background) = exampleLayout
        (collage, vextents) = mkCollage label
        cRender = foldMapCollage cairoPositionedElementRender offsetZero collage
      in
        ((cRender, vextents), background)
  appStateRef <- newIORef $ fix $ \this ->
    AppState
      { appStateLabel = "Source",
        appStateCursor = 0,
        appStatePreMatrix = cachedPreMatrix $ PreMatrix 1 0 (0, 0),
        appStateCollageRender = mkCollageRender (appStateLabel this) }
  cursorPhaser <- createPhaser
  colorPhaser <- createPhaser
  widthPhaser <- createPhaser
  curvaturePhaser <- createPhaser
  _ <- flip Gtk.timeoutAdd 5 $ do
    Gtk.widgetQueueDraw drawArea -- Gtk.postGUIAsync not needed because
                                 -- Gtk.timeoutAdd callback operates inside
                                 -- the GUI thread
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
      ((collageRender, vextents), background) = appStateCollageRender appState
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
      ofs_l = snap $ getExcess (fst viewport') w / 2
      ofs_t = snap $ getExcess (snd viewport') h / 2
    Cairo.setMatrix (Matrix.translate (ofs_l - vl) (ofs_t - vt) matrix')
    let
      fromG :: WithPhase a -> a
      fromG (WithPhase mkX) =
        mkX PhaseCtx
        { phCursor = appStateCursor appState,
          phCursorPhase = cursorPhase,
          phColorPhase = colorPhase,
          phCurvaturePhase = curvaturePhase,
          phWidthPhase = widthPhase }
    cairoRender collageRender fromG
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
                appStateCollageRender = mkCollageRender (appStateLabel this)
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
                appStateCollageRender = mkCollageRender (appStateLabel this)
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
                appStateCollageRender = mkCollageRender (appStateLabel this)
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
                appStateCollageRender = mkCollageRender (appStateLabel this)
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
                  appStateCollageRender = mkCollageRender (appStateLabel this)
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
                  appStateCollageRender = mkCollageRender (appStateLabel this)
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
                  appStateCollageRender = mkCollageRender (appStateLabel this)
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
                  appStateCollageRender = mkCollageRender (appStateLabel this)
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

ubuntuFont :: Centi -> Font
ubuntuFont size = Font "Ubuntu" size FontWeightNormal

exampleLayout :: (Text -> (Collage (CairoElement WithPhase), Extents), Word8 -> Color)
exampleLayout =
  let
    background colorPhase = RGB
      (colorPhase `div` 10)
      (colorPhase `div` 10)
      (colorPhase `div` 10)
    onyellowbkg =
      decorateMargin . DecorationAbove $
        rect (lrtb @Integer 1 1 1 1) (rgb 100 100 0)
    dmbkg = decorateMargin . DecorationBelow $
      rect (phaseConst Nothing) (rgb 100 0 100)
    theCurve = curve
        (rgb 0 0 255)
        (phaseCurvature (Curvature.(subtract 1).(/628)))
        (phaseColor $ \colorPhase -> rgb colorPhase 130 200)
        (directionFrom BottomLeft)
        (phaseWidth ((+1).(/1000)))
        (arrowhead 8 8 2)
    theRectCircle =
      substrate
        (lrtb 1 2 3 4)
        (rect (lrtb 1 2 3 4) (rgb 255 0 0))
        (collageWithMargin (Margin 0 30 0 0)
          (circle (rgb 0 255 0) (phaseWidth $ \w -> Just (10 - (w/300))) 30))
    mkMsgbox msg =
      substrate (lrtb 5 5 5 5) (rect (phaseConst Nothing) (phaseColor $ \colorPhase -> rgb colorPhase 130 200)) $
      substrate (lrtb 1 1 1 1) (rect (phaseConst Nothing) (rgb 0 0 0)) $
      substrate (lrtb 3 3 3 3) (rect (phaseConst Nothing) (rgb 255 255 255)) $
      substrate (lrtb 3 3 3 3) theCurve $
        horizCenter
          (onyellowbkg theRectCircle)
          (dmbkg $
            collageWithMargin (Margin 20 0 0 0)
            (text (ubuntuFont 12) (rgb 0 0 0) msg
            (phaseCursor $ \cursor c -> if c then Just cursor else Nothing)))
    msgbox2 =
      substrate
        (lrtb 5 5 5 5)
        (rect (phaseConst Nothing) (rgb 201 17 38))
        (text (ubuntuFont 12) (rgb 0 0 0) "horiz\nbaseline" (phaseConst Nothing))
    msgboxWithExtents msg =
      let msgbox = horizBaseline (mkMsgbox msg) msgbox2
      in (msgbox, collageExtents msgbox)
  in (msgboxWithExtents, background)
