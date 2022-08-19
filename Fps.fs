namespace MyGame
open System
open Microsoft.Xna.Framework

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

    // Global State
    let state =
        create 0 (TimeSpan 0) 0.0

    // Called on each update
    let update (gameTime:GameTime) =
        state.Frames      <- state.Frames + 1
        state.ElapsedTime <- state.ElapsedTime + gameTime.ElapsedGameTime

        if state.ElapsedTime.TotalSeconds >= 1.0 then
            state.FPS         <- float state.Frames / state.ElapsedTime.TotalSeconds
            state.Frames      <- 0
            state.ElapsedTime <- TimeSpan 0
