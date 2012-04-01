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
{-# LANGUAGE OverloadedStrings #-}
module Cavern.Scene( Scene(..), loadScene ) where

-- -----------------------------------------------------------------------------
import Control.Applicative( (<$>), (<*>) )
import Control.Monad( mzero )
import Data.Aeson( FromJSON(..), Value(..), decode, (.:), (.:?) )
import Data.Maybe( fromMaybe, catMaybes )
import qualified Data.ByteString.Lazy as BSL( readFile )
import System.FilePath.Posix( combine, takeDirectory )
import System.Directory( getCurrentDirectory, setCurrentDirectory )
import Cavern.Image( loadImage )
import Cavern.Stage(
  Stage(..), Layer(..), ImageProp(..), emptyStage, getImageWidth,
  getImageHeight )
import Cavern.Types( Translatable(..) )

-- -----------------------------------------------------------------------------
data DataScene = DataScene
                 { dataSceneLayers :: [DataLayer] }
           deriving( Show )

instance FromJSON DataScene where
  parseJSON (Object v) = do
    layers <- v .: "layers"
    return $ DataScene layers
  parseJSON _ = mzero

-- -----------------------------------------------------------------------------
data DataLayer = DataLayer
                 { dataLayerWidth :: Maybe Int
                 , dataLayerHeight :: Maybe Int
                 , dataLayerProps :: [DataProp] }
               deriving( Show )

instance FromJSON DataLayer where
  parseJSON (Object v) = DataLayer <$>
                         v .:? "width" <*>
                         v .:? "height" <*>
                         v .: "props"
  parseJSON _ = mzero

-- -----------------------------------------------------------------------------
data DataProp = PropStatic DataPropStatic
              | PropFixed DataPropFixed
                deriving( Show )

instance FromJSON DataProp where
  parseJSON (Object v) = do
    val <- sequence [ detect v "static" PropStatic
                    , detect v "fixed" PropFixed ]
    case catMaybes val of
      (x:_) -> return x
      _ -> mzero

      where
        detect obj name mk = obj .:? name
                             >>= return . (maybe Nothing (Just . mk))
  parseJSON _ = mzero


-- -----------------------------------------------------------------------------
data DataPropStatic = DataPropStatic
                      { dataPropStaticImg :: String
                      , dataPropStaticX :: Maybe Int
                      , dataPropStaticY :: Maybe Int }
                  deriving( Show )

instance FromJSON DataPropStatic where
  parseJSON (Object v) = DataPropStatic <$>
                         v .: "img" <*>
                         v .:? "posx" <*>
                         v .:? "posy"
  parseJSON _ = mzero

-- -----------------------------------------------------------------------------
data DataPropFixed = DataPropFixed
                  deriving( Show )

instance FromJSON DataPropFixed where
  parseJSON (Object _) = return DataPropFixed
  parseJSON _ = mzero

-- -----------------------------------------------------------------------------
data Scene = Scene
             { sceneStage :: Stage }
           deriving( Show )

emptyScene :: Scene
emptyScene = Scene emptyStage

-- -----------------------------------------------------------------------------
mkScene :: DataScene -> IO Scene
mkScene dat = do
  layers <- mapM mkLayer (dataSceneLayers dat)
  let stw = maximum . map layerWidth $ layers
      sth = maximum . map layerHeight $ layers
  return $ Scene (Stage stw sth layers)

-- -----------------------------------------------------------------------------
mkLayer :: DataLayer -> IO Layer
mkLayer dat = do
  props <- mapM mkImageProp (dataLayerProps dat)
  ws <- fmap maximum . mapM getImageWidth $ props
  hs <- fmap maximum . mapM getImageHeight $ props
  return $ Layer
    (fromMaybe ws $ dataLayerWidth dat)
    (fromMaybe hs $ dataLayerHeight dat)
    props

-- -----------------------------------------------------------------------------
mkImageProp :: DataProp -> IO ImageProp
mkImageProp (PropStatic dat) = do
  aa <- loadImage (dataPropStaticImg dat)
  let px = fromMaybe 0 $ dataPropStaticX dat
      py = fromMaybe 0 $ dataPropStaticY dat
  return $ ImageProp (return $ moveTo px py aa)

mkImageProp _ = do
  error "invalid prop"

-- -----------------------------------------------------------------------------
loadScene :: FilePath -> IO Scene
loadScene filename = do
  dat <- BSL.readFile filename
  case decode dat of
    Just v -> do
      print v
      curdir <- getCurrentDirectory
      setCurrentDirectory $ combine curdir (takeDirectory filename)
      scn <- mkScene v
      setCurrentDirectory curdir
      return scn
    _ -> do
      print $ "error loading : " ++ filename
      return emptyScene

-- -----------------------------------------------------------------------------
