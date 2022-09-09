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
    Sprite : Texture2D
}

type Movement = {
    Direction : Vector2
}

type Timer<'a> = {
    mutable ElapsedTime : TimeSpan
    TimeFrame           : TimeSpan
    Execute             : 'a -> 'a
    mutable State       : 'a
}
