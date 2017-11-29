module Slay.Gtk.KeyCode where

import Data.Char as Char
import Data.Text as Text
import Data.Word

import Graphics.UI.Gtk.Gdk.Keys (keyName)
import qualified Language.Haskell.TH as TH

return $
  let
    mkDecs1 i name =
      [ TH.PatSynSigD name (TH.ConT ''Word32),
        TH.PatSynD name (TH.PrefixPatSyn[]) TH.ImplBidir (TH.LitP (TH.IntegerL (toInteger i))) ]
    mkDecs i = if valid then mkDecs1 i name else []
      where
        name = TH.mkName (Text.unpack nameStr)
        nameStr = mappend "K_" key
        firstc = Text.head key
        valid = not (Text.null key) && Char.isLetter firstc
        key = keyName i
  in
    [0..2^(16::Int)] >>= mkDecs
