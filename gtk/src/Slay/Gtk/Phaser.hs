module Slay.Gtk.Phaser
  ( Phaser(..),
    createPhaser,
    resetPhaser,
    updatePhaser,
    readPhaser
  ) where

import Data.IORef

newtype Phaser = Phaser (IORef Word)

createPhaser :: IO Phaser
createPhaser = Phaser <$> newIORef 0

resetPhaser :: Phaser -> IO ()
resetPhaser (Phaser r) = writeIORef r 0

updatePhaser :: Phaser -> IO ()
updatePhaser (Phaser r) = atomicModifyIORef' r (\x -> (x + 1, ()))

readPhaser :: Phaser -> (Word -> r) -> IO r
readPhaser (Phaser r) cont = cont <$> readIORef r

