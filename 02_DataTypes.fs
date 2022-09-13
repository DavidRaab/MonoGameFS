namespace MyGame.DataTypes
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

type Entity =
    Entity of int

type Position = {
    Position: Vector2
}

type View = {
    Texture:   Texture2D
    SrcRect:   Rectangle
    IsVisible: bool
    Tint:      Color
    Rotation:  float32
    Origin:    Vector2
    Scale:     Vector2
    Effects:   SpriteEffects
    Layer:     float32
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