namespace MyGame.Systems
open MyGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module View =
    let draw (sb:SpriteBatch) =
        // sb.Begin()
        for e in Component.Entity.all () do
            Component.Position.get e |> Option.iter (fun pos ->
            Component.View.get e     |> Option.iter (fun view ->
                sb.Draw(view.Sprite, pos.Position, Color.White)
            ))
        // sb.End()