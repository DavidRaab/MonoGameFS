namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type FPS = {
    mutable Frames:      int
    mutable ElapsedTime: TimeSpan
    mutable FPS:         float
}

module FPS =
    let create frames time fps = {
        Frames      = frames
        ElapsedTime = time
        FPS         = fps
    }

    let state =
        create 0 (TimeSpan 0) 0.0

    let increment time =
        state.Frames      <- state.Frames + 1
        state.ElapsedTime <- state.ElapsedTime + time

    let calculateFps () =
        if state.ElapsedTime.TotalSeconds >= 1.0 then
            state.FPS         <- float state.Frames / state.ElapsedTime.TotalSeconds
            state.Frames      <- 0
            state.ElapsedTime <- TimeSpan 0

    let updateFps (gameTime:GameTime) =
        increment gameTime.ElapsedGameTime
        calculateFps ()