{- -----------------------------------------------------------------------------
Cavern is a Interactive Fiction Player.
Copyright (C) 2012  Luis Cabellos

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------- -}
module Main( main ) where

-- -----------------------------------------------------------------------------
import qualified Graphics.UI.SDL as SDL( 
  InitFlag(..), Event(..), init, setVideoMode, waitEvent )
import Cavern.Render

-- -----------------------------------------------------------------------------
mainLoop :: IO ()
mainLoop = do
  e <- SDL.waitEvent
  case e of
    SDL.Quit -> return ()
    otherwise -> mainLoop

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  SDL.init [SDL.InitEverything]
  _ <- SDL.setVideoMode 640 480 32 []
  mainLoop

-- -----------------------------------------------------------------------------
testStage :: IO Stage
testStage = do
  aa <- loadImage "data/scene01a.png"
  bb <- loadImage "data/scene01b.png"
  cc <- loadImage "data/scene01c.png"
  dd <- loadImage "data/scene01d.png"
  ss <- loadImage "data/sprite01.png"
  return $! Stage 1000 480
    [ Layer 640 480 [ImageProp (return aa)]
    , Layer 800 480 [ImageProp (return bb)
                    , ImageProp (return $ moveTo 550 200 ss)]
    , Layer 800 480 [ImageProp (return $ moveTo 500 130 cc)]
    , Layer 1000 480 [ImageProp (return $ moveTo 0 309 dd)] ]
  

-- -----------------------------------------------------------------------------
