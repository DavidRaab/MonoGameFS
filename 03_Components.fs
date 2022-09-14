namespace MyGame.Components
open MyGame
open MyGame.DataTypes
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

module Position =
    let create pos =
        { Position = pos }

    let setPosition newPos pos =
        pos.Position <- newPos

    let createXY x y =
        create (Vector2.create x y)

    let add vec2 pos =
        pos.Position <- pos.Position + vec2

    let addX x pos =
        pos.Position <- Vector2.addX x pos.Position

    let addY y pos =
        pos.Position <- Vector2.addY y pos.Position

type Origin =
    | TopLeft
    | Top
    | TopRight
    | Left
    | Center
    | Right
    | BottomLeft
    | Bottom
    | BottomRight
    | Position of float32 * float32

type ViewLayer =
    | BG2
    | BG1
    | FG2
    | FG1
    | UI2
    | UI1

module View =
    let layerToFloat layer =
        match layer with
            | BG2 -> 0.1f
            | BG1 -> 0.2f
            | FG2 -> 0.3f
            | FG1 -> 0.4f
            | UI2 -> 0.5f
            | UI1 -> 0.6f

    /// Generates a View from a whole Texture
    let fromTexture sprite layer = {
        Texture   = sprite
        SrcRect   = Rectangle(0,0,sprite.Width,sprite.Height)
        IsVisible = true
        Tint      = Color.White
        Rotation  = 0f
        Origin    = Vector2.Zero
        Scale     = Vector2.One
        Effects   = SpriteEffects.None
        Layer     = layerToFloat layer
    }

    /// Generates a View from a Sheet by using the selected Sprite
    let fromSheet sheet layer index = {
        Texture   = sheet.Texture
        SrcRect   =
            Array.tryItem index sheet.Sprites |> Option.defaultWith (fun _ ->
                eprintfn "Index [%d] out of Range. Max index is [%d] at\n%s"
                    index (sheet.Sprites.Length-1) (stackTrace 1)
                if   sheet.Sprites.Length > 0
                then Array.get sheet.Sprites 0
                else Rectangle(0,0,64,64)
            )
        IsVisible = true
        Tint      = Color.White
        Rotation  = 0f
        Origin    = Vector2.Zero
        Scale     = Vector2.One
        Effects   = SpriteEffects.None
        Layer     = layerToFloat layer
    }

    let setScale scale (view:View) =
        { view with Scale = scale }

    let setOrigin name (view:View) =
        let width  = float32 view.SrcRect.Width
        let height = float32 view.SrcRect.Height
        let origin =
            let x,y =
                match name with
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
        { view with Origin = origin }

    let setOriginWith name f (view:View) =
        let view = setOrigin name view
        { view with Origin = f view.Origin }

    let flipHorizontal b view =
        match b with
        | true  -> view.Effects <- SpriteEffects.FlipHorizontally
        | false -> view.Effects <- SpriteEffects.None

    let show (view:View) =
        printfn "%A" view
        view

module Sheet =
    let fromWidthHeight (texture:Texture2D) width height =
        let columns = texture.Width  / width
        let rows    = texture.Height / height
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                yield Rectangle(col*width, row*height, width, height)
        |]
        {Texture = texture; Sprites = sprites }

    let fromColumnsRows (texture:Texture2D) columns rows =
        let width  = texture.Width  / columns
        let height = texture.Height / rows
        let sprites = [|
            for row=0 to rows-1 do
            for col=0 to columns-1 do
                yield Rectangle(col*width, row*height, width, height)
        |]
        {Texture = texture; Sprites = sprites }

    let fromSheet sheet idxs =
        let max = sheet.Sprites.Length
        { sheet with
            Sprites = [|
                for idx in idxs do
                    if idx < max then
                        yield sheet.Sprites.[idx]
                    else
                        eprintfn "idx %d out of range. Max index is [%d] at\n%s"
                            idx (sheet.Sprites.Length-1) (stackTrace 0)
            |]
        }

    let fromTexture (texture:Texture2D) = {
        Texture = texture;
        Sprites = [| Rectangle(0,0,texture.Width,texture.Height) |]
    }

module SheetAnimation =
    let create (duration:int) isLoop sheet = {
        Sheet         = sheet
        CurrentSprite = 0
        IsLoop        = isLoop
        ElapsedTime   = TimeSpan.Zero
        Duration      = TimeSpan.FromMilliseconds duration
    }

    let reset anim =
        anim.CurrentSprite <- 0
        anim.ElapsedTime   <- TimeSpan.Zero

    let getSourceRect anim =
        anim.Sheet.Sprites.[anim.CurrentSprite]

    let nextSprite anim =
        let maxSprite = anim.Sheet.Sprites.Length - 1
        if anim.CurrentSprite < maxSprite then
            anim.CurrentSprite <- anim.CurrentSprite + 1
        else
            if anim.IsLoop then anim.CurrentSprite <- 0

    let changeView (anim:SheetAnimation) (view:View) =
        { view with
            Texture = anim.Sheet.Texture
            SrcRect = getSourceRect anim }

module SheetAnimations =
    let create active animations = {
        Animations  = Map animations
        Active      = active
    }

    let hasAnimation str anims =
        Seq.contains str anims.Animations.Keys

    let getAnimation anims =
        anims.Animations[anims.Active]

    let getValidAnimations anims = [
        for animationName in anims.Animations.Keys do
            yield animationName
    ]

    let setAnimation active anims =
        if hasAnimation active anims then
            if anims.Active <> active then
                anims.Active <- active
                SheetAnimation.reset (getAnimation anims)
        else
            eprintfn "No Animation \"%s\" available Animations %A at\n%s"
                active (getValidAnimations anims) (stackTrace 1)

module Movement =
    let create dir =
        { Direction = dir }

    let direction (m:Movement) = m.Direction

    let createXY x y =
        create (Vector2(x,y))
