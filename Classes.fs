namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type RectangleOO(pos:Vector2,color:Color) =
    member val Position = pos
    member val Color    = color with get,set

    member this.Update() =
        this.Color <- Color(this.Color.R+1uy, this.Color.G, this.Color.B, this.Color.A)

    member this.Draw(tex, sb:SpriteBatch) =
        sb.Draw(tex, Rectangle(int this.Position.X, int this.Position.Y, 10, 10), this.Color)