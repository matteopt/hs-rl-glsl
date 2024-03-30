{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad
import Data.Time
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import System.Directory

data State = State
    { wr        :: WindowResources
    , shader    :: Maybe (UTCTime, Shader)
    , totalTime :: Float                   }

screenWidth :: Int
screenWidth = 800

screenHeight :: Int
screenHeight = 800

screenVec2 :: Vector2
screenVec2 = Vector2 (fromIntegral screenWidth) (fromIntegral screenHeight)

shaderPath :: String
shaderPath = "./shader.glsl"

refreshShader :: WindowResources -> Maybe (UTCTime, Shader) -> IO (UTCTime, Shader)
refreshShader wr' Nothing = do
    t <- getModificationTime shaderPath
    s <- loadShader Nothing (Just shaderPath) wr'
    return (t, s)
refreshShader wr' (Just (t1, s)) = do
    t2 <- getModificationTime shaderPath
    if (t2 > t1) then do
        putStrLn "Reloading shader..."
        unloadShader s wr'
        refreshShader wr' Nothing
    else
        return (t1, s)

loop :: State -> IO ()
loop state = do
    guard =<< not <$> windowShouldClose

    newTotalTime <- (+) state.totalTime <$> getFrameTime
    mouse <- getMousePosition
    (t2, s) <- refreshShader state.wr state.shader

    setShaderValue s "resolution" (ShaderUniformVec2 screenVec2)       state.wr
    setShaderValue s "mouse"      (ShaderUniformVec2 mouse)            state.wr
    setShaderValue s "time"       (ShaderUniformFloat state.totalTime) state.wr

    beginDrawing >> do
        beginShaderMode s >> do
            drawRectangle 0 0 screenWidth screenHeight rayWhite
        endShaderMode
    endDrawing

    loop $ state
        { shader    = Just (t2, s)
        , totalTime = newTotalTime }

app :: IO ()
app = do
    wr' <- initWindow screenWidth screenHeight "example"
    setTargetFPS 144
    loop $ State
        { wr        = wr'
        , shader    = Nothing
        , totalTime = 0       }

main :: IO ()
main = app
