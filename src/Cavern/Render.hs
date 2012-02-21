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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cavern.Render( RenderState, runRender, clearScreen, renderText ) where

-- -----------------------------------------------------------------------------
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.State( MonadState, StateT, runStateT, get )
import Data.Text( Text, unpack )
import qualified Graphics.UI.SDL as SDL(
  Surface, InitFlag(..), Color(..), Rect(..), init, setVideoMode, flip,
  blitSurface, mapRGB, fillRect, surfaceGetPixelFormat, getVideoSurface )
import qualified Graphics.UI.SDL.TTF as SDLTTF(
  Font, init, openFont, renderTextBlended, textSize )
import Paths_cavern( getDataFileName )

-- -----------------------------------------------------------------------------
data RenderState = RS { renderFont :: !SDLTTF.Font }

-- -----------------------------------------------------------------------------
mkRenderState :: IO RenderState
mkRenderState = do
  SDL.init [SDL.InitVideo]
  SDLTTF.init
  SDL.setVideoMode 640 480 32 []
  filename <- getDataFileName "GentiumPlus-R.ttf" 
  font <- SDLTTF.openFont filename 14
  return $! RS font

-- -----------------------------------------------------------------------------
newtype Render a = Render
                      { runR :: StateT RenderState IO a }
                    deriving( Functor, Monad, MonadIO
                            , MonadState RenderState )

-- -----------------------------------------------------------------------------
runRender :: MonadIO m => Render a -> RenderState -> m (a, RenderState)
runRender renderf rs = liftIO $ do
  v <- runStateT (runR renderf) rs
  screen <- SDL.getVideoSurface
  SDL.flip screen
  return v

-- -----------------------------------------------------------------------------
io :: IO a -> Render a
io = liftIO

-- -----------------------------------------------------------------------------
getMainBuffer :: Render (SDL.Surface)
getMainBuffer = io SDL.getVideoSurface

-- -----------------------------------------------------------------------------
getMainFont :: Render (SDLTTF.Font)
getMainFont = fmap renderFont $ get

-- -----------------------------------------------------------------------------
clearScreen :: Render ()
clearScreen = do
  screen <- getMainBuffer
  pixel <- io $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 50 0
  io $ SDL.fillRect screen Nothing pixel
  return ()

-- -----------------------------------------------------------------------------
renderText :: Int -> Int -> Text -> Render ()
renderText x y txt = do
  screen <- getMainBuffer
  font <- getMainFont
  let str = unpack txt
  (w,h) <- io $ SDLTTF.textSize font str
  txtBuff <- io $ SDLTTF.renderTextBlended font str (SDL.Color 255 255 255)
  io $ SDL.blitSurface txtBuff Nothing screen (Just $ SDL.Rect x y w h)
  return ()

-- -----------------------------------------------------------------------------
