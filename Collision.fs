module MyGame.Collision
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Box = {
    Texture:  Texture2D
    mutable Position: Vector2
    Size:     Point
    Color:    Color
    LeftX:    int
    RightX:   int
    mutable Movement: Vector2
}

module Box =
    let getRect box =
        Rectangle(Vector2.toPoint box.Position, box.Size)

let mutable state = Unchecked.defaultof<Box>

let loadContent tex =
    state <- {
        Texture  = tex
        Position = Vector2(50f,100f)
        Size     = Point(50,30)
        Color    = Color.Red
        LeftX    = 0
        RightX   = 854
        Movement = Vector2(500f,0f)
    }

let whiteBox = Rectangle(50,50,300,300)

let update (gameTime:GameTime) =
    let rect = Box.getRect state
    if rect.Right > state.RightX then
        state.Movement <- Vector2(-500f,0f)
    elif rect.Left < state.LeftX then
        state.Movement <- Vector2(500f,0f)
    state.Position <- state.Position + Vector2.Multiply (state.Movement, gameTime.ElapsedGameTime)

type Draw =
    | Draw of Texture2D * Rectangle * Color

let draw tex = [|
    if whiteBox.Intersects (Box.getRect state)
    then Draw(tex, whiteBox, Color.Aquamarine)
    else Draw(tex, whiteBox, Color.White)

    Draw(state.Texture, Rectangle(state.Position.ToPoint(),state.Size), state.Color)
|]
