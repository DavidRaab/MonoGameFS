namespace MyGame.Systems
open MyGame
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open MyGame.Timer
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

// View System draws entity
module View =
    let draw (sb:SpriteBatch) =
        let posAndView = [|
            for entity in Entity.positionsAndView.GetCache() do
                match State.Position.get entity, State.View.get entity with
                | ValueSome p, ValueSome v -> p,v
                | _                        -> ()
        |]
        posAndView |> Array.sortInPlaceBy (fun (p,v) -> v.Layer)
        for pos,view in posAndView do
            sb.Draw(
                texture              = view.Sprite,
                destinationRectangle = Rectangle(
                    int pos.Position.X,
                    int pos.Position.Y,
                    view.Sprite.Width,
                    view.Sprite.Height
                ),
                sourceRectangle      = System.Nullable(),
                color                = view.Tint,
                rotation             = view.Rotation,
                origin               = view.Origin,
                effects              = view.Effects,
                layerDepth           = view.Layer
            )

// Moves those who should be moved
module Movement =
    let update (deltaTime:TimeSpan) =
        for entity in Entity.positionsAndMovement.GetCache () do
            entity |> State.Movement.iter (fun mov ->
            entity |> State.Position.map  (fun pos ->
                Position.create (pos.Position + (mov.Direction * float32 deltaTime.TotalSeconds))
            ))

module Timer =
    let mutable state = ResizeArray<Timed<unit>>()

    let addTimer timer =
        state.Add (Timed.get timer)

    let update (deltaTime:TimeSpan) =
        for idx=0 to state.Count-1 do
            match Timed.run deltaTime (state.[idx]) with
            | Pending    -> ()
            | Finished _ -> state.RemoveAt(idx)


