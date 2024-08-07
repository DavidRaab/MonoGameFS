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
    let inline wrap (x:float32) : float32<rad> =
        LanguagePrimitives.Float32WithMeasure<rad> x

    /// A turn describes the angle of a circle between 0 and 1.
    /// So 0.25 or 1/4 is a 1/4 circle or 90 degrees. 0.5 is a half-circle and so on.
    let inline fromTurn (x:float32) : float32<rad> =
        wrap (x * System.MathF.Tau)

    let inline fromDeg (degree:float32<deg>) : float32<rad> =
        wrap(degree * System.MathF.PI / 180.0f<deg>)

    let inline toDeg (radiant:float32<rad>) : float32<deg> =
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
    let toPosition width height origin : Vector2 =
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
    let inline from (t:Transform) : Transform = t

    // Constructors
    let create parent pos rot scale : Transform = from {
        Parent   = parent
        Position = pos
        Rotation = rot
        Scale    = scale
    }

    /// Creates a Transform with the supplied vector2
    let inline fromVector pos : Transform = from {
        Parent   = ValueNone
        Position = pos
        Rotation = 0f<rad>
        Scale    = Vector2.One
    }

    /// Creates a Transform with a position specified as x,y coordinates
    let inline fromPosition x y : Transform = from {
        Parent   = ValueNone
        Position = Vector2(x,y)
        Rotation = 0f<rad>
        Scale    = Vector2.One
    }

    /// Creates a Transform with Position and Rotation
    let inline fromPosRot pos rot : Transform = from {
        Parent   = ValueNone
        Position = pos
        Rotation = rot
        Scale    = Vector2.One
    }

    // Immutable Properties
    /// Creates a new Transform with the provided Parent Transform.
    let withParent parent (t:Transform) : Transform =
        { t with Parent = parent }

    // Mutable Properties
    let inline setPosition newPos (t:Transform) : Transform =
        t.Position <- newPos
        t

    let inline setRotation rotation (t:Transform) : Transform =
        t.Rotation <- rotation
        t

    let inline setRotationVector vector (t:Transform) : Transform =
        t.Rotation <- Vector2.angle vector
        t

    let inline setScale newScale (t:Transform) : Transform =
        t.Scale <- newScale
        t

    /// Adds a vector to the Position
    let inline addPosition vec2 (t:Transform) : unit =
        t.Position <- t.Position + vec2

    // TODO: addLocalTransform - that applies the current rotation

    /// Adds rotation to Transform specified in radiant
    let inline addRotation rotation (t:Transform) : unit =
        t.Rotation <- t.Rotation + rotation

    /// Adds rotation to Transform specified in degree
    let inline addRotationDeg rot (t:Transform) : unit =
        t.Rotation <- t.Rotation + (Radian.fromDeg rot)


module Sprite =
    let create (sprite:Sprite) = sprite

    let fromTexture tex = {
        Texture = tex
        SrcRect = Rectangle(0,0,tex.Width,tex.Height)
    }

    let inline rect    sprite = sprite.SrcRect
    let inline texture sprite = sprite.Texture
    let inline width   sprite = sprite.SrcRect.Width
    let inline height  sprite = sprite.SrcRect.Height

    /// Generates a Sprite array from a Texture2D
    let fromColumnsRows columns rows (texture:Texture2D) : Sprite array =
        let width   = texture.Width  / columns
        let height  = texture.Height / rows
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                yield create {
                    Texture = texture
                    SrcRect = Rectangle(col*width, row*height, width, height)
                }
        |]
        if sprites.Length = 0 then
            failwith "Sheet with 0 Sprites"
        sprites

module View =
    // Turns a Layer into a number. When Drawing the sprites all sprites
    // are sorted by this number. This way we can emulate layers. On the same
    // layer no drawing order can be preserved.
    let layerToFloat layer =
        match layer with
            | BG2 -> 1
            | BG1 -> 2
            | FG2 -> 3
            | FG1 -> 4
            | UI2 -> 5
            | UI1 -> 6

    // Constructors
    let inline create (v:View) : View = v

    /// Generates a View
    let fromSprite origin layer (sprite:Sprite) : View = {
        Sprite    = sprite
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
    let fromSheet layer index (sheet:Sheet) : View = {
        Sprite =
            Array.tryItem index sheet.Sprites |> Option.defaultWith (fun _ ->
                eprintfn "Index [%d] out of Range. Max index is [%d] at\n%s"
                    index (sheet.Sprites.Length-1) (stackTrace 1)
                if   sheet.Sprites.Length > 0
                then Array.get sheet.Sprites 0
                else failwith "Sheet has no Sprites"
            )
        Tint      = Color.White
        Rotation  = 0.0f<rad>
        Origin    = Vector2.Zero
        Scale     = Vector2.One
        Effects   = SpriteEffects.None
        Layer     = layerToFloat layer
    }

    // Immutable Properties
    let withOrigin name (view:View) : View =
        let width  = float32 view.Sprite.SrcRect.Width
        let height = float32 view.Sprite.SrcRect.Height
        let origin = Origin.toPosition width height name
        { view with Origin = origin }

    // Mutable Properties

    let setScale scale (view:View) : View =
        view.Scale <- scale
        view

    let setRotation rot (view:View) : View =
        view.Rotation <- rot
        view

    let setTint tint (view:View) : View =
        view.Tint <- tint
        view

    let flipHorizontal b (view:View) : unit =
        match b with
        | true  -> view.Effects <- SpriteEffects.FlipHorizontally
        | false -> view.Effects <- SpriteEffects.None

    let show (view:View) : View =
        printfn "%A" view
        view

module Sheet =
    let create data : Sheet =
        if data.Sprites.Length = 0 then
            failwithf "Cannot create Sheet. Must at least contain one Sprite."
        data

    let sprite idx (sheet:Sheet) : Sprite voption =
        if idx < sheet.Sprites.Length
        then ValueSome sheet.Sprites.[idx]
        else ValueNone

    let inline length (sheet:Sheet) : int =
        sheet.Sprites.Length

    let inline duration (sheet:Sheet) =
        sheet.FrameDuration * (float sheet.Sprites.Length)

module Sheets =
    let create data : Sheets =
        if not (Map.containsKey data.Default data.Sheets) then
            failwithf "Cannot create Sheets. Default Sheet '%s' does not exists." data.Default
        if data.Sheets.Count = 0 then
            failwithf "Cannot create Sheets. Sheets must have at least one item."
        data

    let addSheet name (sheet: Sheet) sheets : Sheets = {
        sheets with
            Sheets = (Map.add name sheet sheets.Sheets)
    }

    let inline getSheet name (sheets:Sheets) : Sheet option =
        Map.tryFind name sheets.Sheets

    let inline getSheetExn name (sheets:Sheets) : Sheet =
        Map.find name sheets.Sheets

    /// Creates a View from the currently set sprite sheet
    let createView layer origin (sheets:Sheets) : View =
        let sprite = sheets.Sheets.[sheets.Default].Sprites.[0]
        View.create {
            Sprite   = sprite
            Rotation = 0f<rad>
            Tint     = Color.White
            Scale    = Vector2.One
            Effects  = SpriteEffects.None
            Layer    = View.layerToFloat layer
            Origin   = Origin.toPosition (float32 sprite.SrcRect.Width) (float32 sprite.SrcRect.Height) origin
        }

// module Sheet =
//     /// Returns a sprite from a sheet
//     let sprite index sheet =
//         Array.tryItem index sheet.Sprites |> Option.defaultWith (fun _ ->
//             eprintfn "Index [%d] out of Range. Max index is [%d] at\n%s"
//                 index (sheet.Sprites.Length-1) (stackTrace 1)
//             if   sheet.Sprites.Length > 0
//             then Array.get sheet.Sprites 0
//             else failwith "Sheet has 0 Sprites"
//         )

//     let fromWidthHeight width height (texture:Texture2D) =
//         let columns = texture.Width  / width
//         let rows    = texture.Height / height
//         let sprites = [|
//             for row=0 to rows-1 do
//             for col=0 to columns-1 do
//                 yield Sprite.create texture (Rectangle(col*width, row*height, width, height))
//         |]
//         if sprites.Length = 0 then
//             failwith "Sheet with 0 Sprites"
//         { Sprites = sprites }

//     let fromColumnsRows columns rows (texture:Texture2D) =
//         let width   = texture.Width  / columns
//         let height  = texture.Height / rows
//         let sprites = [|
//             for row=0 to rows-1 do
//             for col=0 to columns-1 do
//                 yield Sprite.create texture (Rectangle(col*width, row*height, width, height))
//         |]
//         if sprites.Length = 0 then
//             failwith "Sheet with 0 Sprites"
//         { Sprites = sprites }

//     /// Generates a new Sheet from an existing Sheet by selecting the sprites specified by `idxs`
//     let fromSheet idxs sheet = {
//         Sprites = [|
//             for idx in idxs do
//                 yield sprite idx sheet
//         |]
//     }

//     /// Generates a Sheet from a single texture that will contain a single sprite
//     let fromTexture (texture:Texture2D) = {
//         Sprites = [|
//             Sprite.create texture (Rectangle(0,0,texture.Width, texture.Height))
//         |]
//     }

//     /// Generates a Sheet by directly passing the individual sprites
//     let fromSprites sprites = {
//         Sprites = Array.ofSeq sprites
//     }


module Animation =
    let from (data:Animation) = data

    let create sheets = from {
        Sheets        = sheets
        CurrentSheet  = sheets.Sheets.[sheets.Default] // can throw exception
        CurrentSprite = 0
        ElapsedTime   = TimeSpan.Zero
    }

    let sheets (animation:Animation) : Sheets = animation.Sheets

    let reset (animation:Animation) : unit =
        animation.CurrentSprite <- 0
        animation.ElapsedTime   <- TimeSpan.Zero

    /// Returns the current Sprite in an animation
    let inline currentSprite anim : Sprite voption =
        Sheet.sprite anim.CurrentSprite anim.CurrentSheet

    /// Advance the animation to the next Sprite
    let nextSprite (anim:Animation) : unit =
        let sheet     = anim.CurrentSheet
        let maxSprite = Sheet.length sheet
        if sheet.IsLoop then
            anim.CurrentSprite <- (anim.CurrentSprite + 1) % maxSprite
        else
            if anim.CurrentSprite < maxSprite-1 then
                anim.CurrentSprite <- anim.CurrentSprite + 1

    /// Updates the View to the current Sprite
    let inline updateView (view:View) (anim:Animation) : unit =
        view.Sprite <- anim.CurrentSheet.Sprites.[anim.CurrentSprite]

    /// Switch to another sheet animation
    let switchAnimation name (anim:Animation) : unit =
        match Sheets.getSheet name anim.Sheets with
        | Some sheet ->
            anim.CurrentSheet  <- sheet
            anim.CurrentSprite <- 0
            anim.ElapsedTime   <- TimeSpan.Zero
        | None ->
            let validAnims = System.String.Join(',', Map.keys anim.Sheets.Sheets)
            failwithf "Cannot switch Animation to \"%s\" valid animation are %s" name validAnims

module Movement =
    let inline from (x:Movement) = x

    let inline create dir rot : Movement = {
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
    let inline virtualScale (camera:Camera) : Vector3 =
        let scale = float32 camera.Viewport.Width / float32 camera.VirtualWidth
        Vector3(scale,scale,1f)

    /// Calculates and returns the Matrix of the Camera. This ignores the Matrix field of
    /// the record. It is used to calculate the Matrix to reset the field.
    let calculateMatrix camera =
        // Creates the scale for the virtual resolution the game should use
        Matrix.CreateScale         (virtualScale camera)
        // there is no camera. A camera with position 100px right, 100px top basically means
        // that all game objects must be moved 100px to left and 100px to bottom
        * Matrix.CreateTranslation (Vector3(-camera.Position , 0f))
        // Usually i assume that the origin of the camera is center. This becomes
        // relevant with zooming. Because we want zomming to happen to the center, not TopLeft
        // as the default is. For this to work we first need to additionally subtract the camera
        // origin to all game objects.
        * Matrix.CreateTranslation (-camera.Origin)
        // Then we apply the camera zoom level
        * Matrix.CreateScale       (camera.Zoom, camera.Zoom, 1f)
        // then we finally add the camera origin back
        * Matrix.CreateTranslation (camera.Origin)

    let create (w:int,h:int) (viewport:Viewport) : Camera = {
            Camera.Position = Vector2.create 0f 0f
            Zoom            = 1.0f
            Matrix          = None
            VirtualWidth    = w
            VirtualHeight   = h
            Viewport        = viewport
            Origin          = Vector3(Origin.toPosition (float32 w) (float32 h) Center, 0f)
            MinZoom         = 0.03f
            MaxZoom         = 2.0f
        }

    let withMinMaxZoom min max (camera:Camera) : Camera =
        { camera with MinZoom = min; MaxZoom = max; Matrix = None }

    let withViewport viewport camera : Camera =
        { camera with Viewport = viewport; Matrix = None }

    let setPosition vec (camera:Camera) : unit =
        camera.Position <- vec
        camera.Matrix   <- None

    let setZoom (zoom:float32) (camera:Camera) : unit =
        camera.Zoom   <- clampF zoom camera.MinZoom camera.MaxZoom
        camera.Matrix <- None

    let addZoom (zoom:float32) (camera:Camera) : unit =
        camera.Zoom   <- clampF camera.MinZoom camera.MaxZoom (camera.Zoom + zoom)
        camera.Matrix <- None

    let subtractZoom (zoom:float32) (camera:Camera) : unit =
        camera.Zoom   <- clampF camera.MinZoom camera.MaxZoom (camera.Zoom - zoom)
        camera.Matrix <- None

    let add vec (camera:Camera) : unit =
        camera.Position <- camera.Position + vec
        camera.Matrix   <- None

    let matrix (camera:Camera) : Matrix =
        match camera.Matrix with
        | Some matrix -> matrix
        | None        ->
            let matrix = calculateMatrix camera
            camera.Matrix <- Some matrix
            matrix

    /// Transforms a screen-point defined as Vector2 to the World Position
    let screenToWorld position (camera:Camera) : Vector2 =
        Vector2.Transform(position, Matrix.Invert (matrix camera))

    /// Transform a screen-point defined as Point to the World Position
    let screenPointToWorld (position:Point) (camera:Camera) : Vector2 =
        Vector2.Transform(position.ToVector2(), Matrix.Invert (matrix camera))

    /// Transforms a world-position to screen position
    let worldToScreen (position:Vector2) (camera:Camera) : Vector2 =
        Vector2.Transform(position, matrix camera)
