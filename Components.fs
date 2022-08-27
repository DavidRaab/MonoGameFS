namespace MyGame.Components
open MyGame
open MyGame.DataTypes
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module Position =
    let create pos =
        { Position = pos }

    let position (p:Position) = p.Position

    let createXY x y =
        create (Vector2.create x y)


module View =
    let create sprite =
        {Sprite = sprite }

    let sprite (v:View) = v.Sprite

module Movement =
    let create dir =
        { Direction = dir }

    let direction (m:Movement) = m.Direction

    let createXY x y =
        create (Vector2(x,y))

