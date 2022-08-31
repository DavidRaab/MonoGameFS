namespace MyGame.Systems
open MyGame
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type TimeSpan = System.TimeSpan

module Timer =
    let every timeFrame state f = {
        ElapsedTime = TimeSpan.Zero
        TimeFrame   = timeFrame
        Execute     = f
        State       = state
    }

    let wrap x = every TimeSpan.Zero () (fun dt -> x)

    let run deltaTime (timer:Timer<'a>) =
        timer.ElapsedTime <- timer.ElapsedTime + deltaTime
        if timer.ElapsedTime >= timer.TimeFrame then
            timer.ElapsedTime <- timer.ElapsedTime - timer.TimeFrame
            timer.State       <- timer.Execute timer.State

// View System draws entity
module View =
    let draw (sb:SpriteBatch) =
        // sb.Begin()
        for entity in Entity.positionsAndView.GetCache() do
            entity |> State.Position.iter (fun pos  ->
            entity |> State.View.iter     (fun view ->
                sb.Draw(view.Sprite, pos.Position, Color.White)
            ))
        // sb.End()

// Moves those who should be moved
module Movement =
    let update (deltaTime:TimeSpan) =
        for entity in Entity.positionsAndMovement.GetCache () do
            entity |> State.Movement.iter (fun mov ->
            entity |> State.Position.map  (fun pos ->
                Position.create (pos.Position + (mov.Direction * float32 deltaTime.TotalSeconds))
            ))
