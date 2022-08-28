namespace MyGame.Systems
open MyGame
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

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
    let update (gameTime:GameTime) =
        for entity in Entity.positionsAndMovement do
            entity |> State.Position.iter (fun pos ->
            entity |> State.Movement.iter (fun mov ->
                let newPos = Position.create (pos.Position + (mov.Direction * float32 gameTime.ElapsedGameTime.TotalSeconds))
                entity.addPosition newPos
            ))