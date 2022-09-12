namespace MyGame.DataTypes
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

type Entity =
    Entity of int

type Position = {
    Position : Vector2
}

type View = {
    Sprite:   Texture2D
    Tint:     Color
    Rotation: float32
    Origin:   Vector2
    Scale:    Vector2
    Effects:  SpriteEffects
    Layer:    float32
}

type Movement = {
    Direction : Vector2
}
