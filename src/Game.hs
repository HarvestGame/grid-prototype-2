{-# LANGUAGE TemplateHaskell #-}

module Game ( 
  GameState,
  getCamera,
  renderGame,
  createGameState,
  updateGameState,
  processKeyEvent,
  processMouseMoveEvent,
  processMouseEnterEvent,
  processMouseClickEvent
) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Lens

import Data.Maybe

import Graphics

import Data.Vect.Float.Base

import qualified Graphics.UI.GLFW as GLFW

import Control.Monad.State.Lazy
import GHC.Float

import Debug.Trace

import Data.Either

data Terrain = TypeA | TypeB deriving Eq

data Building = Building {
  bottomLeft :: (Int, Int),
  buildingSize :: (Int, Int)
} deriving Eq

data Unit = Unit {
  centerPosition :: (Float, Float),
  radius :: Float
} deriving Eq

data GameState = GameState {
  _position :: Vec2,
  _pressedKeys :: Set.Set GLFW.Key,
  _width :: Int,
  _height :: Int,
  _grid :: Map.Map (Int, Int) Terrain,
  _rawMousePosition :: Maybe (Float, Float),
  _buildings :: [Building],
  _units :: [Unit],
  _collisionGrid :: Map.Map (Int, Int) [Either Unit Building],
  _selectedBuilding :: Maybe (Either Unit Building)
}

makeLenses ''GameState

isCurrentlyTryingToPlace :: GameState -> Bool
isCurrentlyTryingToPlace = (Set.member GLFW.Key'B) . (^. pressedKeys)

insertBuilding :: Building -> State GameState ()
insertBuilding building = do 
  buildings %= cons building
  let buildingPositions = getBuildingSpots building
  forM_ buildingPositions (\(x,y) -> 
    collisionGrid %= Map.alter helper (x,y)
    ) where
    helper Nothing = Just $ [Right building]
    helper (Just previous) = Just $ (Right building):previous


getBuildingSpots :: Building -> [(Int, Int)]
getBuildingSpots (Building{bottomLeft=(x,y), buildingSize=(dx, dy)}) = do
  xPos <- [x .. (x+dx-1)]
  yPos <- [y .. (y+dy-1)]
  return (xPos, yPos)

getMousePosition :: GameState -> Maybe (Float, Float)
getMousePosition game = do
  (x, y) <- game ^. rawMousePosition
  let Vec2 posX posY = game ^. position
  return (x-posX, y-posY)

getGridLocation :: (Float, Float) -> (Int, Int)
getGridLocation (x,y) = (round $ x/100, round $ y/100)

getCamera :: GameState -> Proj4
getCamera = translation . extendZero . ( ^. position)


terrainSprite :: Terrain -> SpriteMap -> Sprite 
terrainSprite TypeA = aSprite
terrainSprite TypeB = bSprite


mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a,b) = (f a, f b)

renderBuilding :: Building -> Sprite -> DrawRequest
renderBuilding (Building{bottomLeft=(x,y), buildingSize=(dx,dy)}) sprite = 
  DrawRequest {
    location=((fx + fdx/2 - 0.5)*100, (fy + fdy/2 - 0.5)*100), 
    size=(fdx*100, fdy*100),
    sprite=sprite
  } where
    (fx, fy) = mapTuple fromIntegral (x,y)
    (fdx, fdy) = mapTuple fromIntegral (dx, dy)

isValidPlacement :: GameState -> Building -> Bool 
isValidPlacement gameState (Building{bottomLeft=(x,y), buildingSize=(dx, dy)}) = and $ do 
  xPos <- [x .. (x+dx-1)]
  yPos <- [y .. (y+dy-1)]

  let terrainType = Map.findWithDefault TypeA (xPos, yPos) (gameState ^. grid)
  let isColliding = length (Map.findWithDefault [] (xPos, yPos) (gameState ^. collisionGrid)) /= 0

  if isColliding || xPos < 0 || xPos > (gameState ^. width) || yPos < 0 || yPos > (gameState ^. height) || terrainType == TypeB then
    return False
  else 
    return True

currentPlacingBuilding :: GameState -> Maybe Building
currentPlacingBuilding gameState = do 
  guard $ isCurrentlyTryingToPlace gameState
  mousePosition <- getMousePosition gameState
  let (x,y) = getGridLocation mousePosition 
  return Building {bottomLeft = (x-1, y-1), buildingSize=(3,2)}

renderPlacingBuilding :: GameState -> SpriteMap -> [DrawRequest]
renderPlacingBuilding gameState sprites = maybeToList $ do 
  building <- currentPlacingBuilding gameState
  let sprite = (if isValidPlacement gameState building then possibleBuilding sprites else badBuilding sprites)
  return $ renderBuilding building sprite

renderGrid :: GameState -> SpriteMap -> [DrawRequest]
renderGrid game sprites = do 
  xPos <- [0.. (game ^. width)]
  yPos <- [0.. (game ^. height)]

  let terrainType = Map.findWithDefault TypeA (xPos, yPos) (game ^. grid)

  return DrawRequest{location=(fromIntegral xPos * 100, fromIntegral yPos * 100), size=(100,100), sprite=(terrainSprite terrainType sprites)}

renderMouse :: GameState -> SpriteMap -> [DrawRequest]
renderMouse game sprites = do 
  mousePosition <- maybeToList $ getMousePosition game
  return DrawRequest{location=mousePosition, size=(30,30), sprite=(circleSprite sprites)}

renderBuildings :: GameState -> SpriteMap -> [DrawRequest]
renderBuildings game sprites = do 
  building <- game ^. buildings
  let sprite = if (game ^. selectedBuilding) == (Just $ Right building) then
                (selectedBuildingSprite sprites)
              else
                (buildingSprite sprites)

  return $ renderBuilding building sprite

renderUnits :: GameState -> SpriteMap -> [DrawRequest]
renderUnits game sprites = do 
  unit <- game ^. units
  let (x,y) = centerPosition unit
  let sprite = if (game ^. selectedBuilding) == (Just $ Left unit) then
                (selectedUnitSprite sprites)
              else
                (unitSprite sprites)
  return DrawRequest {
    location=(x*100,y*100), 
    size=(radius unit * 200, radius unit * 200),
    sprite=sprite
  }

renderGame :: GameState -> SpriteMap -> [DrawRequest]
renderGame game sprites = concat 
  [ renderGrid game sprites
  , renderBuildings game sprites
  , renderUnits game sprites
  , renderPlacingBuilding game sprites
  , renderMouse game sprites
  ]


createGameState :: GameState
createGameState = GameState (Vec2 0 0.5) Set.empty 100 100 (Map.fromList [((1,0), TypeB)]) Nothing [] [] Map.empty Nothing


updatePosition :: State GameState ()
updatePosition = do 
  currentKeyState <- gets $ (^. pressedKeys)
  if Set.member GLFW.Key'Down currentKeyState then
    position %= (&+ (Vec2 0 10))
  else if Set.member GLFW.Key'Up currentKeyState then
    position %= (&+ (Vec2 0 (-10)))
  else if Set.member GLFW.Key'Left currentKeyState then
    position %= (&+ (Vec2 10 0))
  else if Set.member GLFW.Key'Right currentKeyState then
    position %= (&+ (Vec2 (-10) 0))
  else
    return ()


updateGameState :: State GameState ()
updateGameState = do 
  updatePosition


getUnitCells :: Unit -> [(Int, Int)]
getUnitCells (Unit{centerPosition=(x,y), radius=radius}) = do 
  xPos <- [round (x-radius) .. round (x+radius)]
  yPos <- [round (y-radius) .. round (y+radius)]
  return (xPos,yPos)

insertUnit :: (Int, Int) -> State GameState ()
insertUnit (x,y) = do
  units %= cons unit
  let unitCells = getUnitCells unit
  forM_ unitCells (\(x,y) -> 
    collisionGrid %= Map.alter helper (x,y)
    ) where
    unit = Unit (fromIntegral x, fromIntegral y) 0.49

    helper Nothing = Just $ [Left unit]
    helper (Just previous) = Just $ (Left unit):previous

possiblyCreateUnit :: State GameState ()
possiblyCreateUnit = do 
  gameState <- get 
  let Just theSelectedUnit = gameState ^. selectedBuilding
  when (isRight theSelectedUnit) $ do
    let Right theSelectedBuilding = theSelectedUnit
    let (x,y) = (bottomLeft theSelectedBuilding)
    let spawnPosition = (x+1, y+2)
    insertUnit spawnPosition

processKeyEvent :: GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> State GameState ()
processKeyEvent key _ GLFW.KeyState'Pressed _ = do 
  pressedKeys %= (Set.insert key)
  gameState <- get
  when (key == GLFW.Key'Space && isJust (gameState ^. selectedBuilding)) possiblyCreateUnit

processKeyEvent key _ GLFW.KeyState'Released _ = pressedKeys %= (Set.delete key)
processKeyEvent _ _ _ _ = return ()

processMouseMoveEvent :: Double -> Double -> State GameState ()
processMouseMoveEvent x y = rawMousePosition .= Just (double2Float x, double2Float y)

processMouseClickEvent :: GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> State GameState ()
processMouseClickEvent GLFW.MouseButton'1 GLFW.MouseButtonState'Pressed _ = do
  gameState <- get 
  let maybeBuilding = currentPlacingBuilding gameState
  maybe trySelectBuilding (\building ->
    when (isValidPlacement gameState building) (insertBuilding building)
    ) maybeBuilding
processMouseClickEvent GLFW.MouseButton'2 GLFW.MouseButtonState'Pressed _ = do
  gameState <- get 
  let maybeBuilding = currentPlacingBuilding gameState
  maybe trySelectBuilding (\building ->
    when (isValidPlacement gameState building) (insertBuilding building)
    ) maybeBuilding
processMouseClickEvent _ _ _ = return ()

trySelectBuilding :: State GameState ()
trySelectBuilding = do
  gameState <- get
  let Just mousePosition = getMousePosition gameState
  let gridPosition = getGridLocation mousePosition
  let buildings = Map.findWithDefault [] gridPosition (gameState ^. collisionGrid)

  if (length buildings) == 0 then
    selectedBuilding .= Nothing
  else do
    let theSelectedBuilding = head buildings
    selectedBuilding .= Just theSelectedBuilding


processMouseEnterEvent :: GLFW.CursorState  -> State GameState ()
processMouseEnterEvent GLFW.CursorState'InWindow = return ()
processMouseEnterEvent GLFW.CursorState'NotInWindow = rawMousePosition .= Nothing