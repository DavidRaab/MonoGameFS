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
    mutable Zoom:     float32
    mutable Matrix:   Matrix option // This field is used for caching the matrix
    VirtualWidth:     int
    VirtualHeight:    int
    Viewport:         Viewport
    Origin:           Vector3
    MinZoom:          float32
    MaxZoom:          float32
}

/// A sprite is a selection from a Texture2D
type Sprite = {
    Texture: Texture2D
    SrcRect: Rectangle
}

/// View component. An Entity needs this component to be shown on screen
type View = {
    Entity: Entity
    mutable Sprite:   Sprite
    mutable Rotation: float32<rad>
    mutable Tint:     Color
    mutable Scale:    Vector2
    mutable Effects:  SpriteEffects
    Layer:  int
    Origin: Vector2
}

/// A collection of multiple sprites used in an Animation
type Sheet = {
    Sprites:       Sprite array
    FrameDuration: TimeSpan
    IsLoop:        bool
}

type Sheets = {
    Sheets:  Map<string,Sheet>
    Default: string
}

type Animation = {
    Sheets:                Sheets
    mutable CurrentSheet:  Sheet
    mutable CurrentSprite: int
    mutable ElapsedTime:   TimeSpan
}

type MovementDirection =
    | Relative of Vector2
    | Absolute of position:Vector2 * speed:float32

type Movement = {
    Direction : MovementDirection voption
    Rotation  : float32<rad>      voption
}

