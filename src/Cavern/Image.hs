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
module Cavern.Image( Image(..), loadImage, imgWidth, imgHeight ) where

-- -----------------------------------------------------------------------------
import qualified Graphics.UI.SDL as SDL(
  Surface, surfaceGetWidth, surfaceGetHeight )
import qualified Graphics.UI.SDL.Image as SDL( load )
import Cavern.Types( Translatable(..) )

-- -----------------------------------------------------------------------------
data Image = Image
             { imgX :: ! Int
             , imgY :: ! Int
             , imgSurface :: ! SDL.Surface }

-- -----------------------------------------------------------------------------
instance Translatable Image where
  moveTo x y img = img{ imgX = x, imgY = y }
  translateTo dx dy img = img{ imgX = imgX img + dx
                             , imgY = imgY img + dy }

-- -----------------------------------------------------------------------------
imgWidth :: Image -> Int
imgWidth = SDL.surfaceGetWidth . imgSurface

imgHeight :: Image -> Int
imgHeight = SDL.surfaceGetHeight . imgSurface

-- -----------------------------------------------------------------------------
loadImage :: FilePath -> IO Image
loadImage filename = do
  surface <- SDL.load filename
  return $! Image 0 0 surface

-- -----------------------------------------------------------------------------
