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
    // Calculates position, rotation and scale relative to parent
    // returns an voption because it is called recursively on transform. ValueNone
    // indicates when a parent has no transform defined and recursion ends.
    let rec calculateTransform (me:Transform) =
        match me.Parent with
        | ValueNone        -> ValueSome (me.Position,me.Rotation,me.Scale)
        | ValueSome parent ->
            match State.Transform.get parent with
            | ValueNone        -> ValueNone
            | ValueSome parent ->
                (calculateTransform parent) |> ValueOption.map (fun (pPos,pRot,pScale) ->
                    let scale = Vector2.create (pScale.X * me.Scale.X) (pScale.Y * me.Scale.Y)
                    let pos   = Vector2.Transform(
                        me.Position,
                        Matrix.CreateScale(scale.X, scale.Y, 0f)
                        * Matrix.CreateRotationZ(float32 pRot)       // rotate by parent position
                        * Matrix.CreateTranslation(Vector3(pPos,0f)) // translate by parent position
                    )
                    pos,pRot+me.Rotation,scale
                )

    let draw (sb:SpriteBatch) =
        let transformAndView = [|
            for KeyValue(entity,v) in State.View.visible do
                match State.Transform.get entity with
                | ValueSome t -> (t,v)
                | ValueNone   -> ()
        |]

        transformAndView |> Array.sortInPlaceBy (fun (_,v) -> v.Layer)
        for transform,view in transformAndView do
            match calculateTransform transform with
            | ValueNone                           -> ()
            | ValueSome (position,rotation,scale) ->
                sb.Draw(
                    texture         = view.Sprite.Texture,
                    position        = position,
                    sourceRectangle = view.Sprite.SrcRect,
                    color           = view.Tint,
                    rotation        = float32 (rotation + view.Rotation),
                    origin          = view.Origin,
                    scale           = view.Scale * scale,
                    effects         = view.Effects,
                    layerDepth      = view.Layer
                )

// Moves those who should be moved
module Movement =
    let update (deltaTime:TimeSpan) =
        let fdt = float32 deltaTime.TotalSeconds
        for KeyValue(entity,mov) in State.Movement.Data do
            entity |> State.Transform.iter (fun t ->
                match mov.Direction with
                | ValueNone                        -> ()
                | ValueSome (Relative dir)         -> Transform.addPosition (dir * fdt) t
                | ValueSome (Absolute (pos,speed)) ->
                    let dir = (Vector2.Normalize (pos - t.Position)) * speed
                    Transform.addPosition (dir * fdt) t

                match mov.Rotation with
                | ValueNone     -> ()
                | ValueSome rot -> Transform.addRotation (rot * fdt) t
            )

module Timer =
    let mutable state = ResizeArray<Timed<unit>>()

    let addTimer timer =
        state.Add (Timed.get timer)

    let update (deltaTime:TimeSpan) =
        for idx=0 to state.Count-1 do
            match Timed.run deltaTime (state.[idx]) with
            | Pending    -> ()
            | Finished _ -> state.RemoveAt(idx)

module Animations =
    let update (deltaTime: TimeSpan) =
        for KeyValue(entity,anim) in State.Animation.Data do
            anim.ElapsedTime <- anim.ElapsedTime + deltaTime
            if anim.ElapsedTime > anim.CurrentSheet.FrameDuration then
                anim.ElapsedTime <- anim.ElapsedTime - anim.CurrentSheet.FrameDuration
                Animation.nextSprite anim
                entity |> State.View.iter (fun view ->
                    Animation.updateView view anim
                )

module Drawing =
    let mousePosition (sb:SpriteBatch) (font:SpriteFont) position whereToDraw =
        let world = Camera.screenPointToWorld position State.camera
        sb.DrawString(
            spriteFont = font,
            text       = System.String.Format("Mouse Screen({0},{1}) World({2:0.00},{3:0.00})", position.X, position.Y, world.X, world.Y),
            position   = whereToDraw,
            color      = Color.Black,
            rotation   = 0f,
            origin     = Vector2.Zero,
            scale      = 1f,
            effects    = SpriteEffects.None,
            layerDepth = 0f
        )

    let trackPosition (sb:SpriteBatch) (font:SpriteFont) (entity:Entity) (whereToDraw:Vector2) =
        entity |> State.Transform.iter (fun t ->
            let screen = Camera.worldToScreen t.Position State.camera
            sb.DrawString(
                spriteFont = font,
                text       =
                    System.String.Format("World({0:0.00},{1:0.00}) Screen({2:0},{3:0})",
                        t.Position.X, t.Position.Y,
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
        let angle      = System.MathF.Atan2(hypotenuse.Y, hypotenuse.X)
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

    let rectangle (sprite:Sprite) (thickness:int) color (topLeft:Vector2) (bottomRight:Vector2) (sb:SpriteBatch) =
        let offset     = Vector2(float32 (thickness / 2), 0f)
        let topRight   = Vector2(bottomRight.X, topLeft.Y)
        let bottomLeft = Vector2(topLeft.X, bottomRight.Y)
        let rect       = Rectangle.fromVectors topLeft bottomRight
        sb.Draw(
            texture              = sprite.Texture,
            destinationRectangle = rect,
            sourceRectangle      = sprite.SrcRect,
            color                = color * 0.1f,
            rotation             = 0f,
            origin               = Vector2.Zero,
            effects              = SpriteEffects.None,
            layerDepth           = 0f
        )
        let drawLine = line sprite.Texture thickness color
        sb |> drawLine (topLeft - offset)     (topRight + offset)
        sb |> drawLine  topRight               bottomRight
        sb |> drawLine (bottomRight + offset) (bottomLeft - offset)
        sb |> drawLine  bottomLeft             topLeft
