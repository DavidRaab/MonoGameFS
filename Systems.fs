namespace MyGame.Systems
open MyGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// View System draws entity
module View =
    let draw (sb:SpriteBatch) =
        // sb.Begin()
        for e in Component.Entity.all () do
            Component.Position.get e |> Option.iter (fun pos ->
            Component.View.get e     |> Option.iter (fun view ->
                sb.Draw(view.Sprite, pos.Position, Color.White)
            ))
        // sb.End()

// Moves those who should be moved
module Movement =
    let update (gameTime:GameTime) =
        for e in Component.Entity.all () do
            Component.Position.get e |> Option.iter (fun pos ->
            Component.Movement.get e |> Option.iter (fun mov ->
                let newPos = pos.Position + (mov.Direction * float32 gameTime.ElapsedGameTime.TotalSeconds)
                Component.Position.update newPos e
            ))