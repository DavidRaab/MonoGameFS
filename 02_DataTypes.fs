namespace MyGame.DataTypes
open MyGame.Extensions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

type Entity =
    Entity of int

type Transform = {
    Parent: Entity voption
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
    mutable Zoom:   float
    VirtualWidth:   int
    VirtualHeight:  int
    ViewportWidth:  int
    ViewportHeight: int
    Origin:         Origin
    MinZoom:        float
    MaxZoom:        float
}

type Sprite = {
    Texture: Texture2D
    SrcRect: Rectangle
}

type View = {
    Sprite: Sprite
    Origin: Vector2
    Layer:  float32
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
