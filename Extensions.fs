namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

[<AutoOpen>]
module Extensions =
    module Texture2D =
        let create gd width height data =
            let tex = new Texture2D(gd, width, height)
            tex.SetData data
            tex

    module TimeSpan =
        let oneSecond = TimeSpan.FromSeconds(1.0)

    type Vector2 with
        static member inline Multiply (left:Vector2, right:TimeSpan) =
            Vector2.Multiply(left, float32 right.TotalSeconds)
        static member inline toPoint (vec:Vector2) =
            vec.ToPoint ()