namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Entity = Entity of int

type Position = {
    Entity   : Entity
    Position : Vector2
}

type View = {
    Entity : Entity
    Sprite : Texture2D
}

type Movement = {
    Entity    : Entity
    Direction : Vector2
}
