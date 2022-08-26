namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Entity = Entity of int

type Position = {
    Position : Vector2
}

type View = {
    Sprite : Texture2D
}

type Movement = {
    Direction : Vector2
}
