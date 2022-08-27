namespace MyGame.Systems
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// View System draws entity
module View =
    let draw (sb:SpriteBatch) =
        // sb.Begin()
        for e in Entity.all () do
            Position.get e |> ValueOption.iter (fun pos ->
            View.get e     |> ValueOption.iter (fun view ->
                sb.Draw(view.Sprite, pos.Position, Color.White)
            ))
        // sb.End()

// Moves those who should be moved
module Movement =
    let update (gameTime:GameTime) =
        for e in Entity.all () do
            Position.get e |> ValueOption.iter (fun pos ->
            Movement.get e |> ValueOption.iter (fun mov ->
                let newPos = pos.Position + (mov.Direction * float32 gameTime.ElapsedGameTime.TotalSeconds)
                Position.add newPos e
            ))