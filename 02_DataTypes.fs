namespace MyGame.DataTypes
open MyGame.Extensions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

type Entity =
    Entity of int

type Position = {
    mutable Position: Vector2
}

type View = {
    Texture: Texture2D
    SrcRect: Rectangle
    Origin:  Vector2
    Layer:   float32
    mutable IsVisible: bool
    mutable Tint:      Color
    mutable Rotation:  float32<rad>
    mutable Scale:     Vector2
    mutable Effects:   SpriteEffects
}

type Movement = {
    Direction : Vector2
}

type Sheet = {
    Texture: Texture2D
    Sprites: Rectangle array
}

type SheetAnimation = {
    Sheet:                 Sheet
    mutable CurrentSprite: int
    IsLoop:                bool
    mutable ElapsedTime:   TimeSpan
    Duration:              TimeSpan
}

type SheetAnimations = {
    Animations:     Map<string,SheetAnimation>
    mutable Active: string
}