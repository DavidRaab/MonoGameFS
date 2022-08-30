namespace MyGame.Systems
open MyGame
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

// View System draws entity
module View =
    let draw (sb:SpriteBatch) =
        // sb.Begin()
        for entity in Entity.positionsAndView do
            entity |> State.Position.iter (fun pos  ->
            entity |> State.View.iter     (fun view ->
                sb.Draw(view.Sprite, pos.Position, Color.White)
            ))
        // sb.End()

// Moves those who should be moved
module Movement =
    let update (deltaTime:TimeSpan) =
        for entity in Entity.positionsAndMovement do
            entity |> State.Movement.iter (fun mov ->
            entity |> State.Position.map  (fun pos ->
                Position.create (pos.Position + (mov.Direction * float32 deltaTime.TotalSeconds))
            ))
