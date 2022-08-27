module MyGame.App
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Assets = {
    Font:    Fonts
    Texture: Textures
}

and Textures = {
    Background: Texture2D
    Pixel:      Texture2D
    WhiteBox:   Texture2D
}

and Fonts = {
    Default: SpriteFont
}

type Model = {
    Box:       Entity
    MovingBox: Entity
}

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
            Background = game.Content.Load<Texture2D>("example")
            WhiteBox   = Texture2D.create gd 10 10 (Array.replicate 100 Color.White)
            Pixel      = Texture2D.create gd 1 1 [|Color.White|]
        }
    }

    assets

let initModel assets =
    // ECS System
    let box = State.Entity.create ()
    State.Position.add (Vector2(50f,50f)) box
    State.View.add assets.Texture.WhiteBox box

    let movingBox = State.Entity.create ()
    State.Position.add (Vector2(100f,50f)) movingBox
    State.View.add assets.Texture.WhiteBox movingBox

    let yOffset = 50f
    for x=1 to 75 do
        for y=1 to 40 do
            let e = State.Entity.create ()
            State.Position.add (Vector2.create (float32 x * 11f) (float32 y * 11f + yOffset)) e
            State.View.add     (assets.Texture.WhiteBox) e

    let gameState = {
        Box       = box
        MovingBox = movingBox
    }

    gameState


let update (model:Model) (gameTime:GameTime) (game:MyGame) =
    FPS.update gameTime
    Systems.Movement.update gameTime

    let keyboard = Keyboard.GetState ()
    if keyboard.IsKeyDown Keys.Space then
        State.Movement.add (Vector2(50f,0f)) model.MovingBox

    if keyboard.IsKeyDown Keys.Escape then
        State.Movement.delete model.MovingBox

    if keyboard.IsKeyDown Keys.Right then
        State.Position.get model.MovingBox |> ValueOption.iter (fun pos ->
            let pos = pos.Position + Vector2.Multiply(Vector2(100f,0f), gameTime.ElapsedGameTime)
            State.Position.add pos model.MovingBox
        )

    if keyboard.IsKeyDown Keys.Left then
        State.Position.get model.MovingBox |> ValueOption.iter (fun pos ->
            let pos = pos.Position + Vector2.Multiply(Vector2(-100f,0f), gameTime.ElapsedGameTime)
            State.Position.add pos model.MovingBox
        )


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
    FPS.draw game.Asset.Font.Default game.spriteBatch
    Systems.View.draw game.spriteBatch


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
