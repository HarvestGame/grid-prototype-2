import Data.IORef
import Control.Monad.Except

import Control.Monad.State.Lazy

import Graphics
import Game

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import qualified Foreign.Ptr as Ptr

import Data.Vect.Float.Base
import Data.Vect.Float.OpenGL

import qualified Graphics.GLUtil as GLU

fragmentShaderCode :: String
fragmentShaderCode = unlines [
  "#version 140",

  "uniform sampler2D image;",
  "in vec2 text_coord_temp;",

  "out vec4 color;",

  "void main() {",
  "    color = texture(image, text_coord_temp);",
  "}"
  ]

vertexShaderCode :: String
vertexShaderCode = unlines [
  "#version 140",

  "uniform mat4 screen_transformation;",
  "uniform mat4 camera_transformation;",

  "in vec2 position;",
  "in vec2 text_coord;",
  "out vec2 text_coord_temp;",

  "void main() {",
  "    text_coord_temp = text_coord;",
  "    gl_Position = screen_transformation * camera_transformation * vec4(position, 0.0, 1.0);",
  "}"
  ]

compileAndVerifyShader :: String -> GL.ShaderType -> ExceptT String IO GL.Shader
compileAndVerifyShader source shaderType = do
  shader <- liftIO $ GL.createShader shaderType
  GL.shaderSourceBS shader GL.$= GL.packUtf8 source
  liftIO $ GL.compileShader shader


  status <- GL.get $ GL.compileStatus shader
  info <- GL.get $ GL.shaderInfoLog shader
  if status then
    return shader
  else
    throwError $ "Compile failed for " ++ (show shaderType) ++ info

compileAndVerifyProgram :: String -> String -> ExceptT String IO GL.Program
compileAndVerifyProgram vertexSource fragmentSource = do 
  vertexShader <- compileAndVerifyShader vertexSource GL.VertexShader
  fragmentShader <- compileAndVerifyShader fragmentSource GL.FragmentShader

  program <- liftIO GL.createProgram
  liftIO $ GL.attachShader program vertexShader
  liftIO $ GL.attachShader program fragmentShader
  liftIO $ GL.linkProgram program
  liftIO $ GL.validateProgram program

  linkStatus <- liftIO $ GL.get $ GL.linkStatus program
  validateStatus <- liftIO $ GL.get $ GL.validateStatus program
  programLog <- liftIO $ GL.get $ GL.programInfoLog program 

  if linkStatus && validateStatus then
    return program
  else
    throwError $ "Failed to link or validate, link status: " ++ (show linkStatus) ++ " validate status: " ++ (show validateStatus) ++ " log: " ++ programLog


tickTime :: Double
tickTime = 1/60.0


updateTick :: Double -> GameState -> IO (Double, GameState)
updateTick lastUpdate lastState = do 
  Just currentTime <- GLFW.getTime 
  if lastUpdate < currentTime then
    updateTick (lastUpdate + tickTime) (execState updateGameState lastState)
  else
    return (lastUpdate, lastState)

mainLoop :: GLFW.Window -> GL.Program -> GL.BufferObject -> SpriteMap -> IORef GameState -> Double -> IO ()
mainLoop window program buffer spriteMap gameRef lastUpdate = do 
  shouldClose <- GLFW.windowShouldClose window 
  if shouldClose then
    return ()
  else do
   
    GL.clear [GL.ColorBuffer]


    lastGameState <- readIORef gameRef

    (nextUpdate, nextGameState) <- updateTick lastUpdate lastGameState

    writeIORef gameRef nextGameState

    let cameraMatrix = getCamera nextGameState

    camera_transformation <- GL.get $ GL.uniformLocation program "camera_transformation" :: IO GL.UniformLocation
    cameraGLMatrix <- makeGLMatrix $ Data.Vect.Float.Base.transpose $ cameraMatrix
    GLU.uniformGLMat4 camera_transformation GL.$= cameraGLMatrix

    let drawRequests = renderGame nextGameState spriteMap

    renderDrawRequests drawRequests

    GLFW.swapBuffers window
    GLFW.pollEvents

    mainLoop window program buffer spriteMap gameRef nextUpdate


changeViewPort :: GL.Program -> GLFW.Window -> Int -> Int -> IO ()
changeViewPort program _ width height = do

  screen_transformation <- GL.get $ GL.uniformLocation program "screen_transformation" :: IO GL.UniformLocation
  
  screenGLMatrix <- makeGLMatrix $ scaling $ Vec3  (2/(fromIntegral width)) (2/(fromIntegral height)) 1.0
  GLU.uniformGLMat4 screen_transformation GL.$= screenGLMatrix

  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))


keyCallback :: IORef GameState -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback gameRef _ key keyCode keyState modifiers = ((modifyIORef gameRef) . execState) $ processKeyEvent key keyCode keyState modifiers


mousePosCallback :: IORef GameState -> GLFW.Window -> Double -> Double -> IO ()
mousePosCallback gameRef window x y = do 
  (width, height) <- GLFW.getWindowSize window
  ((modifyIORef gameRef) . execState) $ processMouseMoveEvent (x-(fromIntegral width)/2) ((fromIntegral height)/2 - y)

mouseEnterCallback :: IORef GameState -> GLFW.Window -> GLFW.CursorState -> IO ()
mouseEnterCallback gameRef _ = ((modifyIORef gameRef) . execState) . processMouseEnterEvent

mouseButtonCallback :: IORef GameState -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback gameRef _ button buttonState modifiers = ((modifyIORef gameRef) . execState) $ processMouseClickEvent button buttonState modifiers


init :: ExceptT String IO ()
init = do 
  didInit <- liftIO $ GLFW.init
  if not didInit then
    throwError "GLFW failed to initialize"
  else
    return ()

  liftIO $ GLFW.windowHint $ GLFW.WindowHint'OpenGLDebugContext True

  Just window <- liftIO $ GLFW.createWindow 500 500 "Foo" Nothing Nothing
  liftIO $ GLFW.makeContextCurrent $ Just window

  program <- compileAndVerifyProgram vertexShaderCode fragmentShaderCode

  let game = createGameState
  gameRef <- liftIO $ newIORef game

  spriteMap <- createSpriteMap

  liftIO $ GLFW.setFramebufferSizeCallback window $ Just $ changeViewPort program
  liftIO $ GLFW.setKeyCallback window $ Just $ keyCallback gameRef
  liftIO $ GLFW.setCursorPosCallback window $ Just $ mousePosCallback gameRef
  liftIO $ GLFW.setCursorEnterCallback window $ Just $ mouseEnterCallback gameRef
  liftIO $ GLFW.setMouseButtonCallback window $ Just $ mouseButtonCallback gameRef

  liftIO $ GL.currentProgram GL.$= Just program

  liftIO $ GL.blend GL.$= GL.Enabled
  liftIO $ GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  liftIO $ GL.activeTexture GL.$= GL.TextureUnit 0

  buffer <- liftIO $ GL.genObjectName
  liftIO $ GL.bindBuffer GL.ArrayBuffer GL.$= Just buffer

  image <- liftIO $ GL.get $ GL.uniformLocation program "image"
  (GL.uniform image) GL.$= GL.TextureUnit 0

  positionLocation <- GL.get $ GL.attribLocation program "position"
  GL.vertexAttribArray positionLocation GL.$= GL.Enabled
  GL.vertexAttribPointer positionLocation GL.$= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 16 $ Ptr.wordPtrToPtr 0)

  text_coord <- GL.get $ GL.attribLocation program "text_coord"

  GL.vertexAttribArray text_coord GL.$= GL.Enabled
  GL.vertexAttribPointer text_coord GL.$= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 16 $ Ptr.wordPtrToPtr 8)

  Just startTime <- liftIO $ GLFW.getTime

  liftIO $ mainLoop window program buffer spriteMap gameRef startTime


main :: IO ()
main = do
  result <- runExceptT Main.init

  either (putStrLn) return result
  