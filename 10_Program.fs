module MyGame.App
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open MyGame.Utility
open MyGame.Timer
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// Only load the Keys -- I have my own Input Implementation on top of MonoGame
type Keys = Input.Keys

// Assets
type Assets = {
    Font:       Fonts
    Texture:    Textures
    KnightIdle: Sheet
}
and Textures = {
    Arrow:    Texture2D
    Pixel:    Texture2D
    WhiteBox: Texture2D
}
and Fonts = {
    Default: SpriteFont
}

// Model
type Model = {
    Arrow:      Entity
    Box:        Entity
    MovingBox:  Entity
    MovingBox3: list<Entity>
}

// Type Alias for my game
type MyGame = MonoGame<Assets,Model>


// Initialization of the Game
let init (game:MyGame) =
    game.Graphics.SynchronizeWithVerticalRetrace <- false
    game.IsFixedTimeStep       <- false
    game.TargetElapsedTime     <- TimeSpan.FromSeconds(1.0 / 60.0)
    game.Content.RootDirectory <- "Content"
    game.IsMouseVisible        <- true
    game.SetResolution 854 480


// Loading Assets
let loadAssets (game:MyGame) =
    let load     str = game.Content.Load<Texture2D>(str)
    let loadFont str = game.Content.Load<SpriteFont>(str)

    let gd = game.GraphicsDevice
    let assets = {
        Font = {
            Default = loadFont "Font"
        }
        Texture = {
            Arrow    = load "arrow"
            WhiteBox = Texture2D.create gd 10 10 (Array.replicate 100 Color.White)
            Pixel    = Texture2D.create gd  1  1 [|Color.White|]
        }
        KnightIdle = Sheet.fromColumnsRows (load "FreeKnight/Idle") 10 1
    }

    assets


// Initialize the Game Model
let initModel assets =
    // ECS System
    let box = Entity.init (fun e ->
        e.addPosition (Position.createXY 50f 50f)
        e.addView     (View.fromTexture assets.Texture.WhiteBox FG2)
    )

    let movingBox = Entity.init (fun e ->
        e.addPosition (Position.createXY 100f 50f)
        e.addView     (View.fromTexture assets.Texture.WhiteBox FG2)
    )

    let movingBox3 = Entity.initMany 3 (fun idx e ->
        e.addPosition (Position.createXY (150f + (15f * float32 idx)) 50f)
        e.addView     (View.fromTexture assets.Texture.WhiteBox FG2)
    )

    let arrow = Entity.init (fun e ->
        e.addPosition (Position.createXY 100f 100f)
        e.addView     (
            View.fromTexture assets.Texture.Arrow FG1
            |> View.setOrigin Center
        )
        Systems.Timer.addTimer (Timer.every (TimeSpan.FromSeconds 0.5) () (fun _ dt ->
            State.View.change e (function
                | ValueNone      -> ValueNone
                | ValueSome view -> ValueSome { view with Rotation = view.Rotation + 0.1f }
            )
            State ()
        ))
    )

    let knight = Entity.init (fun e ->
        e.addPosition    (Position.createXY 200f 200f)
        e.addView        (View.fromSheet assets.KnightIdle FG1 0)
        e.addSheetAnimations (
            SheetAnimations.create "Idle" [
                "Idle", SheetAnimation.create true 100 true assets.KnightIdle
            ]
        )
    )

    let knight = Entity.init (fun e ->
        e.addPosition    (Position.createXY 200f 100f)
        e.addView        (View.fromSheet assets.KnightIdle FG1 0 |> View.setScale (Vector2.create 3f 3f))
        e.addSheetAnimations (
            SheetAnimations.create "Idle" [
                "Idle", SheetAnimation.create true 100 true assets.KnightIdle
            ]
        )
    )

    let boxes = ResizeArray<_>()
    let yOffset = 50f
    for x=1 to 75 do
        for y=1 to 40 do
            boxes.Add (Entity.init (fun box ->
                box.addPosition (Position.createXY (float32 x * 11f) (float32 y * 11f + yOffset))
                box.addView     (View.fromTexture assets.Texture.WhiteBox BG1)
            ))


    Systems.Timer.addTimer (Timer.every (TimeSpan.FromSeconds 1.0) false (fun state dt ->
        let vec = if state then Vector2.right 10f else Vector2.left 10f
        for box in boxes do
            State.Position.map (fun pos -> {pos with Position = pos.Position + vec}) box
        State (not state)
    ))

    Systems.Timer.addTimer (Timer.every (TimeSpan.FromSeconds 1.0) () (fun _ _ ->
        System.GC.Collect ()
        State ()
    ))

    let gameState = {
        Arrow      = arrow
        Box        = box
        MovingBox  = movingBox
        MovingBox3 = movingBox3
    }
    gameState


// A Fixed Update implementation that tuns at the specified fixedUpdateTiming
let fixedUpdateTiming = TimeSpan.FromSeconds (1.0 / 60.0)
let fixedUpdate model deltaTime =
    Systems.Movement.update deltaTime
    Systems.Timer.update deltaTime
    Systems.SheetAnimations.update deltaTime

    if Keyboard.isPressed Keys.Space then
        // Toggles between automatic moving and stopping
        let toggleMovement = function
            | ValueNone   -> ValueSome (Movement.create (Vector2.right 50f))
            | ValueSome x -> ValueNone
        State.Movement.change model.MovingBox toggleMovement
        List.iter (fun e -> State.Movement.change e toggleMovement) model.MovingBox3

    if Keyboard.isKeyDown Keys.Right then
        State.Position.get model.MovingBox |> ValueOption.iter (fun pos ->
            let pos = Position.create (pos.Position + Vector2.Multiply(Vector2.right 100f, deltaTime))
            model.MovingBox.addPosition pos
        )

    if Keyboard.isKeyDown Keys.Left then
        State.Position.get model.MovingBox |> ValueOption.iter (fun pos ->
            let pos = Position.create (pos.Position + Vector2.Multiply(Vector2.left 100f, deltaTime))
            model.MovingBox.addPosition pos
        )

    // Resets the Keyboard State
    Keyboard.nextState ()

    model


let mutable fixedUpdateElapsedTime = TimeSpan.Zero
let update (model:Model) (gameTime:GameTime) (game:MyGame) =
    // Get current keyboard state and add it to our KeyBoard module
    // This way we ensure that fixedUpdate has correct keyboard state between
    // fixedUpdate calls and not just from the current update.
    let keyboard = Input.Keyboard.GetState ()
    Keyboard.addKeys (keyboard.GetPressedKeys())

    // Close Game
    if keyboard.IsKeyDown Keys.Escape then
        game.Exit ()

    let deltaTime = gameTime.ElapsedGameTime
    FPS.update deltaTime

    // FixedUpdate Handling
    fixedUpdateElapsedTime <- fixedUpdateElapsedTime + deltaTime
    let model =
        if fixedUpdateElapsedTime >= fixedUpdateTiming then
            fixedUpdateElapsedTime <- fixedUpdateElapsedTime - fixedUpdateTiming
            fixedUpdate model fixedUpdateTiming
        else
            model

    (*
    // Vibration through Triggers
    // printfn "%f %f" gamePad.Triggers.Left gamePad.Triggers.Right
    GamePad.SetVibration(0,
        gamePad.Triggers.Left,
        gamePad.Triggers.Right
    ) |> ignore

    if keyboard.IsKeyDown Keys.Space then
        ignore <| GamePad.SetVibration(0, 1.0f, 1.0f)

    if GamePad.isPressed gamePad.Buttons.A then
        printfn "Pressed A"

    if GamePad.isPressed gamePad.Buttons.Back || keyboard.IsKeyDown Keys.Escape then
        game.Exit()
    *)

    model


let draw (model:Model) (gameTime:GameTime) (game:MyGame) =
    game.GraphicsDevice.Clear(Color.CornflowerBlue)
    game.spriteBatch.Begin ()
    FPS.draw game.Asset.Font.Default game.spriteBatch
    Systems.View.draw game.spriteBatch
    game.spriteBatch.End ()

    (* // Full Example for all parameters
    game.spriteBatch.DrawString(
        spriteFont = game.Asset.Font.Default,
        text       = "Hello, World!",
        position   = Vector2(100f, 100f),
        color      = Color.White,
        rotation   = 0f,
        origin     = Vector2.Zero,
        scale      = Vector2.One,
        effects    = SpriteEffects.None,
        layerDepth = 0f
    )
    *)


// Run MonoGame Application
[<EntryPoint;System.STAThread>]
let main argv =
    using (new MonoGame<Assets,Model>(init, loadAssets, initModel, update, draw)) (fun game ->
        game.Run()
    )
    1
