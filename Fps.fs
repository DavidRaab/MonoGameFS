module MyGame.FPS
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type FPS = {
    mutable Frames:      int
    mutable ElapsedTime: TimeSpan
    mutable FPS:         float
}

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

let draw font (sb:SpriteBatch) =
    sb.DrawString(
        spriteFont = font,
        text       = String.Format("FPS: {0:0.00}", state.FPS),
        position   = Vector2(3f, 3f),
        color      = Color.Yellow,
        rotation   = 0f,
        origin     = Vector2.Zero,
        scale      = 1.33f,
        effects    = SpriteEffects.None,
        layerDepth = 0f
    )

