namespace MyGame.Systems
open MyGame
open MyGame.Extensions
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
            if view.IsVisible then
                sb.Draw(
                    texture         = view.Texture,
                    position        = pos.Position,
                    scale           = view.Scale,
                    sourceRectangle = view.SrcRect,
                    color           = view.Tint,
                    rotation        = view.Rotation,
                    origin          = view.Origin,
                    effects         = view.Effects,
                    layerDepth      = view.Layer
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

module SheetAnimations =
    let update (deltaTime: TimeSpan) =
        for entity in State.SheetAnimations.Entities do
            entity |> State.SheetAnimations.iter (fun anims ->
                let anim = SheetAnimations.getCurrentAnimation anims
                anim.ElapsedTime <- anim.ElapsedTime + deltaTime
                if anim.ElapsedTime > anim.Duration then
                    anim.ElapsedTime <- anim.ElapsedTime - anim.Duration
                    SheetAnimation.nextSprite anim
                // TODO: Optimization
                //   Theoretically only must execute when nextSprite was called
                //   but then the first frame when setting a new animation is skipped.
                //   Could be mutable or setting a new animation must be revisited.
                State.View.map (SheetAnimation.changeView anim) entity
            )

module Debug =
    let mousePosition (sb:SpriteBatch) (font:SpriteFont)  =
        let state = Input.Mouse.GetState()
        let world = Camera.screenToWorld (Vector2.create (float32 state.X) (float32 state.Y)) State.camera
        sb.DrawString(
            spriteFont = font,
            text       = System.String.Format("Mouse Screen({0},{1}) World({2:0.00},{3:0.00})", state.X, state.Y, world.X, world.Y),
            position   = Vector2(3f, 460f),
            color      = Color.Black,
            rotation   = 0f,
            origin     = Vector2.Zero,
            scale      = 1f,
            effects    = SpriteEffects.None,
            layerDepth = 0f
        )

    let trackPosition (sb:SpriteBatch) (font:SpriteFont) (entity:Entity) (whereToDraw:Vector2) =
        entity |> State.Position.iter (fun pos ->
            let screen = Camera.worldToScreen pos.Position State.camera
            sb.DrawString(
                spriteFont = font,
                text       =
                    System.String.Format("World({0:0.00},{1:0.00}) Screen({2:0},{3:0})",
                        pos.Position.X, pos.Position.Y,
                        screen.X, screen.Y
                    ),
                position   = whereToDraw,
                color      = Color.Black,
                rotation   = 0f,
                origin     = Vector2.Zero,
                scale      = 1f,
                effects    = SpriteEffects.None,
                layerDepth = 0f
            )
        )

    let line (texture:Texture2D) (thickness:int) color (start:Vector2) (stop:Vector2) (sb:SpriteBatch) =
        let hypotenuse = (stop - start)
        let length     = int (Vector2.length hypotenuse)
        let angle      = atan2 hypotenuse.Y hypotenuse.X
        sb.Draw(
            texture         = texture,
            position        = start,
            scale           = Vector2.One,
            sourceRectangle = Rectangle(0,0,length,thickness),
            color           = color,
            rotation        = angle,
            origin          = Vector2(0f, float32 thickness / 2f),
            effects         = SpriteEffects.None,
            layerDepth      = 0f
        )

    let rectangle (texture:Texture2D) (thickness:int) color (topLeft:Vector2) (bottomRight:Vector2) (sb:SpriteBatch) =
        let offset     = Vector2(float32 (thickness / 2), 0f)
        let topRight   = Vector2(bottomRight.X, topLeft.Y)
        let bottomLeft = Vector2(topLeft.X, bottomRight.Y)
        let drawLine   = line texture thickness color
        sb |> drawLine (topLeft - offset)     (topRight + offset)
        sb |> drawLine  topRight               bottomRight
        sb |> drawLine (bottomRight + offset) (bottomLeft - offset)
        sb |> drawLine  bottomLeft             topLeft
