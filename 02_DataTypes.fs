namespace MyGame.DataTypes
open MyGame.Extensions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

type Entity =
    Entity of int

type Transform = {
    mutable Position:  Vector2
    mutable Direction: Vector2
    mutable Scale:     Vector2
}

type Origin =
    | TopLeft
    | Top
    | TopRight
    | Left
    | Center
    | Right
    | BottomLeft
    | Bottom
    | BottomRight
    | Position of float32 * float32

type ViewLayer =
    | BG2
    | BG1
    | FG2
    | FG1
    | UI2
    | UI1

type Camera = {
    mutable CameraPosition: Vector2
    mutable Zoom: float
    Width:        int
    Height:       int
    Origin:       Origin
    MinZoom:      float
    MaxZoom:      float
}

type View = {
    Texture: Texture2D
    SrcRect: Rectangle
    Origin:  Vector2
    Layer:   float32
    mutable IsVisible: bool
    mutable Tint:      Color
    mutable Rotation:  float<rad>
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
