module Graphics (
  Sprite(..),
  SpriteMap(..),
  DrawRequest(..),
  renderDrawRequests,
  createSprite,
  createSpriteMap
) where


import Data.List
import Control.Monad
import Control.Monad.Except

import qualified Graphics.Rendering.OpenGL as GL

import qualified Data.Vector.Storable as SVector

import qualified Codec.Picture as Picture


data Sprite = Sprite {
  imageFilename :: String,
  textureObject :: GL.TextureObject
} deriving (Eq, Show)


data SpriteMap = SpriteMap {
  aSprite :: Sprite,
  bSprite :: Sprite,
  circleSprite :: Sprite,
  possibleBuilding :: Sprite,
  badBuilding :: Sprite,
  buildingSprite :: Sprite,
  selectedBuildingSprite :: Sprite,
  unitSprite :: Sprite,
  selectedUnitSprite :: Sprite
}

createSpriteMap :: ExceptT String IO SpriteMap
createSpriteMap = do
  SpriteMap <$> createSprite "img/A.png" 
            <*> createSprite "img/B.png" 
            <*> createSprite "img/Circle.png" 
            <*> createSprite "img/PossibleBuilding.png" 
            <*> createSprite "img/BadBuilding.png" 
            <*> createSprite "img/Building.png" 
            <*> createSprite "img/SelectedBuilding.png"
            <*> createSprite "img/Unit.png"
            <*> createSprite "img/SelectedUnit.png"

data DrawRequest = DrawRequest {
  location :: (Float, Float),
  size :: (Float, Float),
  sprite :: Sprite
} deriving Show

loadTexture :: String -> ExceptT String IO GL.TextureObject
loadTexture filename = do
  texture <- liftIO $ GL.genObjectName
  liftIO $ GL.textureBinding GL.Texture2D GL.$= Just texture

  liftIO $ GL.textureFilter  GL.Texture2D GL.$= ((GL.Nearest, Just GL.Nearest), GL.Nearest)
  liftIO $ GL.textureWrapMode GL.Texture2D GL.S GL.$= (GL.Mirrored, GL.ClampToBorder)
  liftIO $ GL.textureWrapMode GL.Texture2D GL.T GL.$= (GL.Mirrored, GL.ClampToBorder)

  image <- ExceptT $ Picture.readImage filename
  helper image 
  liftIO $ GL.generateMipmap' GL.Texture2D
  return texture where

  helper :: Picture.DynamicImage -> ExceptT String IO ()
  helper (Picture.ImageRGBA8 image) = do 
    liftIO $ SVector.unsafeWith (Picture.imageData image) (\ptr -> 
      GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D (fromIntegral $ Picture.imageWidth image) (fromIntegral $ Picture.imageHeight image)) 0 (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
      )
    return ()

  helper _ = throwError "Wrong image format"

createSprite :: String -> ExceptT String IO Sprite
createSprite filename = do 
  texture <- loadTexture filename
  return $ Sprite {imageFilename=filename, textureObject=texture}

convertToData :: DrawRequest -> [Float]
convertToData (DrawRequest{location=(x,y), size=(width, height)}) = 
  [ (x-width/2), (y-height/2), 0,1
  , (x+width/2), (y-height/2), 1,1
  , (x+width/2), (y+height/2), 1,0

  , (x-width/2), (y-height/2), 0,1
  , (x-width/2), (y+height/2), 0,0
  , (x+width/2), (y+height/2), 1,0
  ]

drawGroup :: [DrawRequest] -> IO ()
drawGroup requests = do 
  let texture = textureObject $ sprite $ head requests

  let vertexData = concat $ map convertToData requests

  GL.textureBinding GL.Texture2D GL.$= Just texture

  let storableArray = SVector.fromList vertexData :: SVector.Vector Float

  SVector.unsafeWith storableArray (\ptr ->
    GL.bufferData GL.ArrayBuffer GL.$= (fromIntegral $ (length vertexData)*4, ptr, GL.StaticDraw)
    )

  GL.drawArrays GL.Triangles 0 (fromIntegral (length vertexData))
  return ()

renderDrawRequests :: [DrawRequest] -> IO ()
renderDrawRequests drawRequests = do
  let groupedRequests = groupBy (\a b -> (sprite a == sprite b)) drawRequests

  forM_ groupedRequests drawGroup
