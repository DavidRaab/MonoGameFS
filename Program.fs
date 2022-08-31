module MyGame.App
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open MyGame.Entity
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

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
    Box:       Entity
    MovingBox: Entity
    MoveBoxes: Timer<bool>
}

// 1. Run once after time
// 2. Run for a duration
// 3. Run periodically after a time

module Timed =
    let fixedUpdateTiming =
        TimeSpan.FromSeconds(1.0 / 60.0)

    // Do something after a specific time is elapsed
    let runEveryTimeFrame timeFrame =
        let mutable elapsedTime = TimeSpan.Zero
        fun f deltaTime->
            elapsedTime <- elapsedTime + deltaTime
            if elapsedTime >= timeFrame then
                f ()
                elapsedTime <- elapsedTime - timeFrame

    let runWithState state =
        let mutable state = state
        fun f ->
            state <- f state

    let runGC =
        runEveryTimeFrame TimeSpan.oneSecond (fun () ->
            System.GC.Collect ()
        )

    let runFixedUpdateTiming =
        runEveryTimeFrame fixedUpdateTiming


// Type Alias for my game
type MyGame = MonoGame<Assets,Model>


let init (game:MyGame) =
    game.Graphics.SynchronizeWithVerticalRetrace <- false
    game.IsFixedTimeStep       <- false
    game.TargetElapsedTime     <- TimeSpan.FromSeconds(1.0 / 60.0)
    game.Content.RootDirectory <- "Content"
    game.IsMouseVisible        <- true
    game.SetResolution 854 480


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


let initModel assets =
    // ECS System
    let box = Entity.create ()
    box.addPosition (Position.createXY 50f 50f)
    box.addView     (View.create assets.Texture.WhiteBox)

    let movingBox = Entity.create ()
    movingBox.addPosition (Position.createXY 100f 50f)
    movingBox.addView     (View.create assets.Texture.WhiteBox)

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
        Box       = box
        MovingBox = movingBox
        MoveBoxes = moveBoxes
    }
    gameState


let pressedKeys = ResizeArray<Keys>()
let fixedUpdate model deltaTime =
    Systems.Movement.update deltaTime
    // Run GC periodically
    Timed.runGC deltaTime
    Systems.Timer.run deltaTime model.MoveBoxes |> ignore

    let keyboard = KeyboardState(pressedKeys.ToArray())
    pressedKeys.Clear()

    if keyboard.IsKeyDown Keys.Space then
        model.MovingBox.addMovement (Movement.create (Vector2.right 50f))

    if keyboard.IsKeyDown Keys.Escape then
        model.MovingBox.deleteMovement ()

    if keyboard.IsKeyDown Keys.Right then
        State.Position.get model.MovingBox |> ValueOption.iter (fun pos ->
            let pos = Position.create (pos.Position + Vector2.Multiply(Vector2.right 100f, deltaTime))
            model.MovingBox.addPosition pos
        )

    if keyboard.IsKeyDown Keys.Left then
        State.Position.get model.MovingBox |> ValueOption.iter (fun pos ->
            let pos = Position.create (pos.Position + Vector2.Multiply(Vector2.left 100f, deltaTime))
            model.MovingBox.addPosition pos
        )

    model


let update (model:Model) (gameTime:GameTime) (game:MyGame) =
    let deltaTime = gameTime.ElapsedGameTime
    FPS.update deltaTime
    pressedKeys.AddRange (Keyboard.GetState().GetPressedKeys())

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

    (*
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
