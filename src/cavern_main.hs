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
module Main( main, test01 ) where

-- -----------------------------------------------------------------------------
import Control.Monad( forM_ )
import qualified Graphics.UI.SDL as SDL(
  InitFlag(..), Event(..), init, setVideoMode, waitEvent )
import System.Posix.Unistd( sleep )
import Cavern.Render(
  Camera(..), mkRenderState, runRender, clearScreen, renderStage )
import Cavern.Scene( Scene(..), loadScene )
import Cavern.Stage( Stage(..) )

-- -----------------------------------------------------------------------------
--mainLoop :: Stage -> IO ()
mainLoop a st = do
  runRender (clearScreen >> renderStage st (Camera 10 0) ) a  
  e <- SDL.waitEvent
  case e of
    SDL.Quit -> return ()
    _ -> mainLoop a st

-- -----------------------------------------------------------------------------
main :: IO ()
main = do
  a <- mkRenderState
  st <- fmap sceneStage $ loadScene "data/scene01.json"
  mainLoop a st

-- -----------------------------------------------------------------------------
test01 :: IO ()
test01 = do
  a <- mkRenderState
  st <- fmap sceneStage $ loadScene "data/scene01.json"
  forM_ [20,40..360] $ \i -> do
    _ <- runRender (clearScreen >> renderStage st (Camera i 0) ) a
    sleep 1

-- -----------------------------------------------------------------------------
