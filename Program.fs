namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Fonts = {
    Default: SpriteFont
}

type Textures = {
    Background: Texture2D
}

type Assets = {
    Font:    Fonts
    Texture: Textures
}

type GameState = GameState

module GameState =
    let create () = GameState

type MyGame = Game1<Assets,GameState>

module App =
    let init (game:MyGame) =
        game.SetResolution 854 480
        GameState.create ()

    let loadContent (game:MyGame) = {
            Font = {
                Default = game.Content.Load<SpriteFont>("Font")
            }
            Texture = {
                Background = game.Content.Load<Texture2D>("example")
            }
        }

    let update (model:GameState) (gameTime:GameTime) (game:MyGame) =
        FPS.update gameTime
        //printfn "%A" FPS.state

        let gamePad  = GamePad.GetState PlayerIndex.One
        let keyboard = Keyboard.GetState ()

        // Vibration through Triggers
        printfn "%f %f" gamePad.Triggers.Left gamePad.Triggers.Right
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

        model

    let draw (model:GameState) (gameTime:GameTime) (game:MyGame) =
        let width, height = game.GetResolution ()

        game.spriteBatch.Draw(
            game.Asset.Texture.Background,
            Rectangle(0, 0, width, height),
            Color.White
        )

        game.spriteBatch.DrawString(
            game.Asset.Font.Default,
            "This is the text to be displayed",
            Vector2(100f, 100f),
            Color.White
        );

    // Run MonoGame Application
    [<EntryPoint;STAThread>]
    let main argv =
        using (new Game1<Assets,GameState>(init, loadContent, update, draw)) (fun game ->
            game.Content.RootDirectory <- "Content"
            game.IsMouseVisible        <- true
            game.Run()
        )
        1
