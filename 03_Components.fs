namespace MyGame.Components
open MyGame
open MyGame.DataTypes
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module Position =
    let create pos =
        { Position = pos }

    let position (p:Position) = p.Position
    let withPosition newPos pos =
        { pos with Position = newPos }

    let createXY x y =
        create (Vector2.create x y)

    let add vec2 pos =
        { pos with Position = pos.Position + vec2 }

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

module View =
    let create sprite tint rotation origin scale effect depth = {
            Sprite   = sprite
            Tint     = tint
            Rotation = rotation
            Origin   = origin
            Scale    = scale
            Effects  = effect
            Depth    = depth
        }

    let fromSprite sprite depth =
        create sprite Color.White 0f Vector2.Zero Vector2.Zero SpriteEffects.None depth

    let setOrigin name view =
        let width  = float32 view.Sprite.Width
        let height = float32 view.Sprite.Height
        let origin =
            let x,y =
                match name with
                | TopLeft     ->         0f,          0f
                | Top         -> width / 2f,          0f
                | TopRight    -> width     ,          0f
                | Left        ->         0f, height / 2f
                | Center      -> width / 2f, height / 2f
                | Right       -> width     , height / 2f
                | BottomLeft  ->         0f, height
                | Bottom      -> width / 2f, height
                | BottomRight -> width     , height
            Vector2(x,y)
        { view with Origin = origin }

    let sprite (v:View) = v.Sprite

module Movement =
    let create dir =
        { Direction = dir }

    let direction (m:Movement) = m.Direction

    let createXY x y =
        create (Vector2(x,y))
