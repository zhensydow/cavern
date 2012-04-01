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
module Cavern.Render(
  -- * Render Monad
  RenderState, runRender, mkRenderState,  clearScreen, renderText,
  renderRectangle, renderImage, loadImage, renderStage,
  -- * Types
  TextSpan(..), Rectangle(..), Image(..), Translatable(..), Camera(..),
  -- * Colors
  black, white, red, green, blue
  ) where

-- -----------------------------------------------------------------------------
import Control.Monad( forM_ )
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.State( MonadState, StateT, runStateT, get )
import Data.Text( Text, unpack )
import Data.Word( Word8 )
import qualified Graphics.UI.SDL as SDL(
  Surface, InitFlag(..), Color(..), Rect(..), init, setVideoMode, setCaption,
  flip, blitSurface, mapRGB, fillRect, surfaceGetPixelFormat, getVideoSurface )
import qualified Graphics.UI.SDL.TTF as SDLTTF(
  Font, init, openFont, renderTextBlended, textSize )
import Cavern.Image( Image(..), loadImage )
import Cavern.Stage( Stage(..), Layer(..), ImageProp(..) )
import Cavern.Types( Translatable(..) )
import Paths_cavern( getDataFileName )

-- -----------------------------------------------------------------------------
black, white, red, green, blue :: (Word8, Word8, Word8)
black = (0,0,0)
white = (255,255,255)
red = (255,0,0)
green = (0,255,0)
blue = (0,0,255)

-- -----------------------------------------------------------------------------
data TextSpan = TextSpan
                { txtX :: ! Int
                , txtY :: ! Int
                , txtColor :: !(Word8, Word8, Word8)
                , txtData :: ! Text }

-- -----------------------------------------------------------------------------
data Rectangle = Rectangle
                 { rectX :: ! Int
                 , rectY :: ! Int
                 , rectW :: ! Int
                 , rectH :: ! Int
                 , rectColor :: !(Word8, Word8, Word8) }

-- -----------------------------------------------------------------------------
data Camera = Camera
              { cameraX :: ! Int
              , cameraY :: ! Int }
              deriving( Show )

-- -----------------------------------------------------------------------------
data RenderState = RS { renderFont :: !SDLTTF.Font }

-- -----------------------------------------------------------------------------
mkRenderState :: IO RenderState
mkRenderState = do
  _ <- SDL.init [SDL.InitVideo]
  _ <- SDLTTF.init
  _ <- SDL.setVideoMode 640 480 32 []
  SDL.setCaption "cavern" ""
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
  _ <- io $ SDL.fillRect screen Nothing pixel
  return ()

-- -----------------------------------------------------------------------------
renderText :: TextSpan -> Render ()
renderText (TextSpan x y (r,g,b) txt) = do
  screen <- getMainBuffer
  font <- getMainFont
  let str = unpack txt
  (w,h) <- io $ SDLTTF.textSize font str
  txtBuff <- io $ SDLTTF.renderTextBlended font str (SDL.Color r g b)
  _ <- io $ SDL.blitSurface txtBuff Nothing screen (Just $ SDL.Rect x y w h)
  return ()

-- -----------------------------------------------------------------------------
renderRectangle :: Rectangle -> Render ()
renderRectangle (Rectangle x y w h (r,g,b)) = do
  screen <- getMainBuffer
  pixel <- io $ SDL.mapRGB (SDL.surfaceGetPixelFormat screen) r g b
  _ <- io $ SDL.fillRect screen (Just $ SDL.Rect x y w h) pixel
  return ()

-- -----------------------------------------------------------------------------
renderImage :: Image -> Render ()
renderImage (Image x y img) = do
  screen <- getMainBuffer
  _ <- io $ SDL.blitSurface img Nothing screen (Just $ SDL.Rect x y 0 0)
  return ()

-- -----------------------------------------------------------------------------
renderStage :: Stage -> Camera -> Render ()
renderStage stage camera = do
  forM_ (stageLayers stage) $ \layer -> do
    let lw = fromIntegral $ layerWidth layer - 640
        tx = if stw == 0
             then 0
             else round $ (fromIntegral posx) * (lw / stw)
    forM_ (layerImages layer) $ \prop -> do
      img <- io $ getImage prop
      renderImage $ translateTo (-tx) (-posy) $ img
  return ()

    where
      stw = (fromIntegral $ stageWidth stage - 640) :: Double
      posx = cameraX camera
      posy = cameraY camera

-- -----------------------------------------------------------------------------
