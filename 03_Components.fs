namespace MyGame.Components
open MyGame
open MyGame.DataTypes
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

(* Design Philosophy

Functions that mutate a record must return `unit`. This way it is easy to distinguish functions
that return a new record without mutating anything.

An exception to this rule are function that begin with `set` usually to explicitly mutate
a property. These functions must mutate the entry (to be consistent) and still return
the original mutated record. This way Function piping can be used, and it is still obvious
that an record gets mutated.

So there are three cases of functions

1. function:    record -> unit   // Mutates record
2. function:    record -> record // Returns a new record without mutating any field
3  setFunction: record -> record // Mutates the record and returns the record
*)

module Radian =
    let inline wrap (x:float32) =
        LanguagePrimitives.Float32WithMeasure<rad> x

    /// A turn describes the angle of a circle between 0 and 1.
    /// So 0.25 or 1/4 is a 1/4 circle or 90 degrees. 0.5 is a half-circle and so on.
    let inline fromTurn x =
        wrap (x * System.MathF.Tau)

    let inline fromDeg (degree:float32<deg>) =
        wrap(degree * System.MathF.PI / 180.0f<deg>)

    let inline toDeg (radiant:float32<rad>) =
        (float32 radiant) * 180.0f<deg> / System.MathF.PI

module Origin =
    /// <summary>
    /// Expects a `width`, `height` and an `origin`. Returns a `Vector2` that
    /// represents the Position of the choosen Origin.
    ///
    /// <code lang="fsharp">
    /// Origin.toPosition 100f 100f Center = Vector2( 50f,50f)
    /// Origin.toPosition 100f 100f Right  = Vector2(100f,50f)
    /// </code>
    /// </summary>
    let toPosition width height origin =
        let x,y =
            match origin with
            | TopLeft        ->         0f,          0f
            | Top            -> width / 2f,          0f
            | TopRight       -> width     ,          0f
            | Left           ->         0f, height / 2f
            | Center         -> width / 2f, height / 2f
            | Right          -> width     , height / 2f
            | BottomLeft     ->         0f, height
            | Bottom         -> width / 2f, height
            | BottomRight    -> width     , height
            | Position (x,y) -> x,y
        Vector2(x,y)

module Transform =
    /// A default constructor that basically does nothing. It expects a record
    /// and returns it immediately. The whole purpose of this is because sometimes
    /// type-inference on records can break. By writing `Transform.from { ... }`
    /// its like an additional type declaration. The Compiler/IDE immediately knows
    /// which record you wanna create and which fields are needed. Also reads
    /// nicely in written code.
    let inline from (t:Transform) = t

    // Constructors
    let create parent pos rot scale = from {
        Parent   = parent
        Position = pos
        Rotation = rot
        Scale    = scale
    }

    let empty = from {
        Parent   = ValueNone
        Position = Vector2.Zero
        Rotation = 0f<rad>
        Scale    = Vector2.One
    }

    /// Creates a Transform with the supplied vector2
    let inline fromVector pos   = { empty with Position = pos }

    /// Creates a Transform with a position specified as x,y coordinates
    let inline fromPosition x y = { empty with Position = Vector2.create x y }

    /// Creates a Transform with Position and Rotation
    let inline fromPosRot pos rot = {
        empty with
            Position = pos
            Rotation = rot
    }

    // Immutable Properties
    /// Creates a new Transform with the provided Parent Transform.
    let withParent parent (t:Transform) =
        { t with Parent = parent }

    // Mutable Properties
    let inline setPosition newPos (t:Transform) =
        t.Position <- newPos
        t

    let inline setRotation rotation (t:Transform) =
        t.Rotation <- rotation
        t

    let inline setRotationVector vector (t:Transform) =
        t.Rotation <- Vector2.angle vector
        t

    let inline setScale newScale (t:Transform) =
        t.Scale <- newScale
        t

    /// Adds a vector to the Position
    let inline addPosition vec2 (t:Transform) =
        t.Position <- t.Position + vec2

    // TODO: addLocalTransform - that applies the current rotation

    /// Adds rotation to Transform specified in radiant
    let inline addRotation rotation (t:Transform) =
        t.Rotation <- t.Rotation + rotation

    /// Adds rotation to Transform specified in degree
    let inline addRotationDeg rot (t:Transform) =
        t.Rotation <- t.Rotation + (Radian.fromDeg rot)


module Sprite =
    let inline from (s:Sprite) = s

    let create tex rect = {
        Texture = tex
        SrcRect = rect
    }

    let fromTexture tex = {
        Texture = tex
        SrcRect = Rectangle(0,0,tex.Width,tex.Height)
    }

    let inline rect    sprite = sprite.SrcRect
    let inline texture sprite = sprite.Texture
    let inline width   sprite = sprite.SrcRect.Width
    let inline height  sprite = sprite.SrcRect.Height

module View =
    // Turns a Layer into a number. When Drawing the sprites all sprites
    // are sorted by this number. This way we can emulate layers. On the same
    // layer no drawing order can be preserved.
    let inline layerToFloat layer =
        match layer with
            | BG2 -> 0.1f
            | BG1 -> 0.2f
            | FG2 -> 0.3f
            | FG1 -> 0.4f
            | UI2 -> 0.5f
            | UI1 -> 0.6f

    // Constructors
    let inline from (v:View) = v

    /// Generates a View
    let fromSprite origin layer sprite = {
        Sprite    = sprite
        IsVisible = true
        Tint      = Color.White
        Rotation  = 0.0f<rad>
        Origin    = Origin.toPosition (float32 sprite.SrcRect.Width) (float32 sprite.SrcRect.Height) origin
        Scale     = Vector2.One
        Effects   = SpriteEffects.None
        Layer     = layerToFloat layer
    }

    let fromSpriteTop    = fromSprite Top
    let fromSpriteRight  = fromSprite Right
    let fromSpriteBottom = fromSprite Bottom
    let fromSpriteLeft   = fromSprite Left
    let fromSpriteCenter = fromSprite Center

    /// Generates a View from a Sheet by using the selected Sprite
    let fromSheet layer index sheet = {
        Sprite =
            Array.tryItem index sheet.Sprites |> Option.defaultWith (fun _ ->
                eprintfn "Index [%d] out of Range. Max index is [%d] at\n%s"
                    index (sheet.Sprites.Length-1) (stackTrace 1)
                if   sheet.Sprites.Length > 0
                then Array.get sheet.Sprites 0
                else failwith "Sheet has no Sprites"
            )
        IsVisible = true
        Tint      = Color.White
        Rotation  = 0.0f<rad>
        Origin    = Vector2.Zero
        Scale     = Vector2.One
        Effects   = SpriteEffects.None
        Layer     = layerToFloat layer
    }

    // Immutable Properties
    let withOrigin name (view:View) =
        let width  = float32 view.Sprite.SrcRect.Width
        let height = float32 view.Sprite.SrcRect.Height
        let origin = Origin.toPosition width height name
        { view with Origin = origin }

    // Mutable Properties

    let setScale scale (view:View) =
        view.Scale <- scale
        view

    let setRotation rot (view:View) =
        view.Rotation <- rot
        view

    let setTint tint (view:View) =
        view.Tint <- tint
        view

    let flipHorizontal b view =
        match b with
        | true  -> view.Effects <- SpriteEffects.FlipHorizontally
        | false -> view.Effects <- SpriteEffects.None

    let show (view:View) =
        printfn "%A" view
        view

module Sheet =
    /// Returns a sprite from a sheet
    let sprite index sheet =
        Array.tryItem index sheet.Sprites |> Option.defaultWith (fun _ ->
            eprintfn "Index [%d] out of Range. Max index is [%d] at\n%s"
                index (sheet.Sprites.Length-1) (stackTrace 1)
            if   sheet.Sprites.Length > 0
            then Array.get sheet.Sprites 0
            else failwith "Sheet has 0 Sprites"
        )

    let fromWidthHeight width height (texture:Texture2D) =
        let columns = texture.Width  / width
        let rows    = texture.Height / height
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                yield Sprite.create texture (Rectangle(col*width, row*height, width, height))
        |]
        if sprites.Length = 0 then
            failwith "Sheet with 0 Sprites"
        { Sprites = sprites }

    let fromColumnsRows columns rows (texture:Texture2D) =
        let width   = texture.Width  / columns
        let height  = texture.Height / rows
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                yield Sprite.create texture (Rectangle(col*width, row*height, width, height))
        |]
        if sprites.Length = 0 then
            failwith "Sheet with 0 Sprites"
        { Sprites = sprites }

    /// Generates a new Sheet from an existing Sheet by selecting the sprites specified by `idxs`
    let fromSheet idxs sheet = {
        Sprites = [|
            for idx in idxs do
                yield sprite idx sheet
        |]
    }

    /// Generates a Sheet from a single texture that will contain a single sprite
    let fromTexture (texture:Texture2D) = {
        Sprites = [|
            Sprite.create texture (Rectangle(0,0,texture.Width, texture.Height))
        |]
    }

    /// Generates a Sheet by directly passing the individual sprites
    let fromSprites sprites = {
        Sprites = Array.ofSeq sprites
    }


module SheetAnimation =
    let create (duration:int<ms>) isLoop sheet = {
        Sheet         = sheet
        CurrentSprite = 0
        IsLoop        = isLoop
        ElapsedTime   = TimeSpan.Zero
        Duration      = TimeSpan.FromMilliseconds (float (duration / 1<ms>))
    }

    // FIXME: See SheetAnimation.copy
    let copy sheet = {
        Sheet         = sheet.Sheet
        CurrentSprite = sheet.CurrentSprite
        IsLoop        = sheet.IsLoop
        ElapsedTime   = sheet.ElapsedTime
        Duration      = sheet.Duration
    }

    let sheet anim =
        anim.Sheet

    let fullDuration anim =
        anim.Duration * float anim.Sheet.Sprites.Length

    let reset anim =
        anim.CurrentSprite <- 0
        anim.ElapsedTime   <- TimeSpan.Zero

    /// Returns the current Sprite in an animation
    let inline currentSprite anim =
        anim.Sheet.Sprites.[anim.CurrentSprite]

    /// Advance the animation to the next Sprite
    let nextSprite anim =
        let maxSprite = anim.Sheet.Sprites.Length
        if anim.IsLoop then
            anim.CurrentSprite <- (anim.CurrentSprite + 1) % maxSprite
        else
            if anim.CurrentSprite < maxSprite-1 then
                anim.CurrentSprite <- anim.CurrentSprite + 1

    /// Returns a new View with the current Sprite of an SheetAnimation
    let withCurrentSprite (anim:SheetAnimation) (view:View) =
        { view with Sprite = currentSprite anim }

module SheetAnimations =
    let create active animations =
        let animations = Map animations
        let validAnims = Map.keys animations
        if Seq.contains active validAnims then {
            Animations = animations
            Active     = active
        }
        else
            failwithf "Cannot set active Animation to \"%s\" valid animations are %A" active validAnims

    // FIXME: SheetAnimations must be separated into two components. One Component
    //        just describes the Animation and is immutable. It does not contain
    //        any data for the running animation.
    //        Then from the SheetAnimations there is a mutable structure that contains
    //        the current sprite for the given entity.
    //        Currently I just copy SheetAnimations to a new Structure for an easy fix.
    let copy sheets = {
        Animations = sheets.Animations |> Map.map (fun name sheet -> SheetAnimation.copy sheet)
        Active     = sheets.Active
    }

    let hasAnimation str anims =
        Seq.contains str anims.Animations.Keys

    let getAnimationsNames anims = [
        for animationName in anims.Animations.Keys do
            yield animationName
    ]

    let getAnimationExn name anims =
        match Map.tryFind name anims.Animations with
        | Some anim -> anim
        | None      -> failwithf "Cannot find animation \"%s\" available animations %A" name (getAnimationsNames anims)

    let getCurrentAnimation anims =
        getAnimationExn anims.Active anims

    let setAnimation active anims =
        if hasAnimation active anims then
            if anims.Active <> active then
                SheetAnimation.reset (getCurrentAnimation anims)
                anims.Active <- active
        else
            eprintfn "No Animation \"%s\" available Animations %A at\n%s"
                active (getAnimationsNames anims) (stackTrace 1)

    let toView layer anims =
        let anim = getCurrentAnimation anims
        SheetAnimation.sheet anim
        |> View.fromSheet layer (anim.CurrentSprite)

module Movement =
    let inline from (x:Movement) = x

    let inline create dir rot = {
        Direction = ValueSome dir
        Rotation  = ValueSome rot
    }

    let empty = {
        Direction = ValueNone
        Rotation  = ValueNone
    }

    /// creates a movement containing a direction
    let inline fromDirection dir = from { Direction = (ValueSome (Relative dir)); Rotation = ValueNone }
    /// creates a movement containing a rotation
    let inline fromRotation  rot = from { Direction = ValueNone; Rotation = (ValueSome rot) }

    /// create a movement that moves to position
    let moveTo position speed = from {
        Direction = (ValueSome (Absolute (position,speed)))
        Rotation  =  ValueNone
    }

    /// get .Direction of Component
    let inline direction (m:Movement) = m.Direction
    /// get .Rotation of Component
    let inline rotation  (m:Movement) = m.Rotation

    let withDirection         dir (mov:Movement) = { mov with Direction = ValueSome (Relative dir) }
    let withPosition          pos (mov:Movement) = { mov with Direction = ValueSome (Absolute pos) }
    let withRotationPerSecond rot (mov:Movement) = { mov with Rotation  = ValueSome rot            }

module Camera =
    let virtualScale camera =
        let scale = float32 camera.Viewport.Width / float32 camera.VirtualWidth
        Vector3(scale,scale,1f)

    /// Calculates and returns the Matrix of the Camera. This ignores the Matrix field of
    /// the record. It is used to calculate the Matrix to reset the field.
    let calculateMatrix camera =
        let origin = Origin.toPosition (float32 camera.VirtualWidth) (float32 camera.VirtualHeight) camera.Origin

        Matrix.CreateScale         (virtualScale camera)
        * Matrix.CreateTranslation (Vector3(-camera.Position , 0f))
        * Matrix.CreateTranslation (Vector3(-origin, 0f))
        * Matrix.CreateScale       (float32 camera.Zoom, float32 camera.Zoom, 1f)
        * Matrix.CreateTranslation (Vector3(origin, 0f))

    let create (w,h) viewport = {
        Camera.Position = Vector2.create 0f 0f
        Zoom            = 1.0
        Matrix          = None
        VirtualWidth    = w
        VirtualHeight   = h
        Viewport        = viewport
        Origin          = Center
        MinZoom         = 0.03
        MaxZoom         = 2.0
    }

    let withMinMaxZoom min max camera =
        { camera with MinZoom = min; MaxZoom = max; Matrix = None }

    let withViewport viewport camera =
        { camera with Viewport = viewport; Matrix = None }

    let setPosition vec camera =
        camera.Position <- vec
        camera.Matrix   <- None

    let setZoom zoom camera =
        camera.Zoom   <- clamp zoom camera.MinZoom camera.MaxZoom
        camera.Matrix <- None

    let addZoom addition camera =
        camera.Zoom   <- clamp camera.MinZoom camera.MaxZoom (camera.Zoom + addition)
        camera.Matrix <- None

    let subtractZoom subtraction camera =
        camera.Zoom   <- clamp camera.MinZoom camera.MaxZoom (camera.Zoom - subtraction)
        camera.Matrix <- None

    let add vec camera =
        camera.Position <- camera.Position + vec
        camera.Matrix   <- None

    let matrix camera =
        match camera.Matrix with
        | Some matrix -> matrix
        | None        ->
            let matrix = calculateMatrix camera
            camera.Matrix <- Some matrix
            matrix

    let screenToWorld position camera =
        Vector2.Transform(position, Matrix.Invert (matrix camera))

    let screenPointToWorld (position:Point) camera =
        Vector2.Transform(position.ToVector2(), Matrix.Invert (matrix camera))

    let worldToScreen position camera =
        Vector2.Transform(position, matrix camera)
