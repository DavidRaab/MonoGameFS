module MyGame.FPS
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type FPS = {
    mutable Updates:     int
    mutable Draws:       int
    mutable ElapsedTime: TimeSpan
    mutable UpdateFPS:   float
    mutable DrawFPS:     float
}

let create updates draws time ufps dfps = {
    Updates     = updates
    Draws       = draws
    ElapsedTime = time
    UpdateFPS   = ufps
    DrawFPS     = dfps
}

// Global State
let state =
    create 0 0 TimeSpan.Zero 0.0 0.0

// Called on each update
let update (gameTime:GameTime) =
    state.Updates     <- state.Updates + 1
    state.ElapsedTime <- state.ElapsedTime + gameTime.ElapsedGameTime

    if state.ElapsedTime >= TimeSpan.oneSecond then
        state.UpdateFPS   <- float state.Updates / state.ElapsedTime.TotalSeconds
        state.DrawFPS     <- float state.Draws   / state.ElapsedTime.TotalSeconds
        state.Updates     <- 0
        state.Draws       <- 0
        state.ElapsedTime <- TimeSpan.Zero

let draw font (sb:SpriteBatch) =
    state.Draws <- state.Draws + 1
    sb.DrawString(
        spriteFont = font,
        text       = String.Format("Update/Draw: {0:0.00} {1:0.00}", state.UpdateFPS, state.DrawFPS),
        position   = Vector2(3f, 3f),
        color      = Color.Yellow,
        rotation   = 0f,
        origin     = Vector2.Zero,
        scale      = 1.33f,
        effects    = SpriteEffects.None,
        layerDepth = 0f
    )

