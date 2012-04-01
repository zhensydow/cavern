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
module Cavern.Stage(
  -- * Functions
  emptyStage, getImageX, getImageY, getImageWidth, getImageHeight,
  -- * Types
  Stage(..), Layer(..), ImageProp(..)
  ) where

-- -----------------------------------------------------------------------------
import Cavern.Image( Image(..), imgWidth, imgHeight )

-- -----------------------------------------------------------------------------
data ImageProp = ImageProp
             { getImage :: IO Image }

instance Show ImageProp where
  show _ = "ImageProp"

getImageX :: ImageProp -> IO Int
getImageX = fmap imgX . getImage

getImageY :: ImageProp -> IO Int
getImageY = fmap imgX . getImage

getImageWidth :: ImageProp -> IO Int
getImageWidth = fmap imgWidth . getImage

getImageHeight :: ImageProp -> IO Int
getImageHeight = fmap imgHeight . getImage

-- -----------------------------------------------------------------------------
data Layer = Layer
             { layerWidth :: ! Int
             , layerHeight :: ! Int
             , layerImages :: [ImageProp] }
           deriving( Show )

-- -----------------------------------------------------------------------------
data Stage = Stage
             { stageWidth :: ! Int
             , stageHeight :: ! Int
             , stageLayers :: [Layer] }
           deriving( Show )

-- -----------------------------------------------------------------------------
emptyStage :: Stage
emptyStage = Stage 0 0 []

-- -----------------------------------------------------------------------------
