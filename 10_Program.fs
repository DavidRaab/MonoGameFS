module MyGame.App
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open MyGame.Utility
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// Only load the Keys -- I have my own Input Implementation on top of MonoGame
type Keys = Input.Keys

type Assets = {
    Font:    Fonts
    Texture: Textures
}

and Textures = {
    Pixel:    Texture2D
    WhiteBox: Texture2D
}

and Fonts = {
    Default: SpriteFont
}

type Model = {
    Box:        Entity
    MovingBox:  Entity
    MovingBox3: list<Entity>
    MoveBoxes:  Timer<bool>
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
    let gd = game.GraphicsDevice
    let assets = {
        Font = {
            Default = game.Content.Load<SpriteFont>("Font")
        }
        Texture = {
            WhiteBox = Texture2D.create gd 10 10 (Array.replicate 100 Color.White)
            Pixel    = Texture2D.create gd 1 1 [|Color.White|]
        }
    }

    assets


// Initialize the Game Model
let initModel assets =
    // ECS System
    let box = Entity.init (fun e ->
        e.addPosition (Position.createXY 50f 50f)
        e.addView     (View.create assets.Texture.WhiteBox)
    )

    let movingBox = Entity.init (fun e ->
        e.addPosition (Position.createXY 100f 50f)
        e.addView     (View.create assets.Texture.WhiteBox)
    )

    let movingBox3 = Entity.initMany 3 (fun idx e ->
        e.addPosition (Position.createXY (150f + (15f * float32 idx)) 50f)
        e.addView     (View.create assets.Texture.WhiteBox)
    )

    let boxes = ResizeArray<_>()
    let yOffset = 50f
    for x=1 to 75 do
        for y=1 to 40 do
            let box = Entity.create ()
            box.addPosition (Position.createXY (float32 x * 11f) (float32 y * 11f + yOffset))
            box.addView     (View.create assets.Texture.WhiteBox)
            boxes.Add box

    let moveBoxes = Systems.Timer.every (TimeSpan.FromSeconds 1.0) false (fun state ->
        let vec = if state then Vector2.right 10f else Vector2.left 10f
        for box in boxes do
            State.Position.map (fun pos -> {pos with Position = pos.Position + vec}) box
        not state
    )

    let gameState = {
        Box        = box
        MovingBox  = movingBox
        MovingBox3 = movingBox3
        MoveBoxes  = moveBoxes
    }
    gameState


// A Fixed Update implementation. This currently runs 60 times per second.
// Can be configured in Utility.fs -> Timed.fixedUpdateTiming
let fixedUpdate model deltaTime =
    Systems.Movement.update deltaTime
    // Run GC periodically
    Timed.runGC deltaTime
    Systems.Timer.run deltaTime model.MoveBoxes |> ignore

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
    let mutable model = model
    Timed.runFixedUpdateTiming (fun () ->
        model <- fixedUpdate model Timed.fixedUpdateTiming
    ) deltaTime

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
    game.spriteBatch.Begin ()
    game.GraphicsDevice.Clear(Color.CornflowerBlue)
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
