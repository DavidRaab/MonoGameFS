module MyGame.Rectangles
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Rect = {
    mutable Position: Vector2
    mutable Color:    Color
}

let inline create pos color =
    { Position = pos; Color = color }

let state = ResizeArray<Rect>()

let loadContent () =
    let yOffset = 0f
    for x=1 to 75 do
        for y=1 to 40 do
            state.Add (create (Vector2 (float32 x * 11f, float32 y * 11f + yOffset)) Color.White)

let update (gameTime:GameTime) =
    // let red   = Color.DarkRed.ToVector3 ()
    // let delta = float32 gameTime.ElapsedGameTime.TotalSeconds
    for rect in state do
        rect.Color <- Color(rect.Color.R+1uy, rect.Color.G, rect.Color.B, rect.Color.A)

let draw tex (sb:SpriteBatch) =
    for rect in state do
        sb.Draw(
            tex,
            Rectangle(int rect.Position.X + FPS.state.Draws, int rect.Position.Y, 10, 10),
            rect.Color
        )