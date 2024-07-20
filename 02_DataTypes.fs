namespace MyGame.DataTypes
open MyGame.Extensions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

[<Struct>]
type Entity =
    Entity of int

type Transform = {
    Parent: Entity voption
    mutable Position: Vector2
    mutable Scale:    Vector2
    mutable Rotation: float32<rad>
}

[<Struct>]
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
    mutable Position: Vector2
    mutable Zoom:     float
    mutable Matrix:   Matrix option // This field is used for caching the matrix
    VirtualWidth:     int
    VirtualHeight:    int
    Viewport:         Viewport
    Origin:           Origin
    MinZoom:          float
    MaxZoom:          float
}

type Sprite = {
    Texture: Texture2D
    SrcRect: Rectangle
}

type View = {
    mutable IsVisible: bool
    Sprite: Sprite
    mutable Rotation:  float32<rad>
    mutable Tint:      Color
    mutable Scale:     Vector2
    mutable Effects:   SpriteEffects
    Layer:  float32
    Origin: Vector2
}

type MovementDirection =
    | Relative of Vector2
    | Absolute of position:Vector2 * speed:float32

type Movement = {
    Direction : MovementDirection voption
    Rotation  : float32<rad>      voption
}

type Sheet = {
    Sprites: Sprite array
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
