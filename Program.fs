namespace MyGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type Model = Model

module Model =
    let create () = Model

type MyGame = Game1<Model>

module App =
    let init (game:MyGame) =
        game.SetResolution 800 600
        Model.create ()

    let loadContent (game:MyGame) =
        ()

    let update (model:Model) (gameTime:GameTime) (game:MyGame) =
        FPS.updateFps gameTime
        printfn "%A" FPS.state

        let gamePad  = GamePad.GetState PlayerIndex.One
        let keyboard = Keyboard.GetState ()

        if GamePad.isPressed gamePad.Buttons.Back || keyboard.IsKeyDown Keys.Escape then
            game.Exit()

        model

    let draw (model:Model) (gameTime:GameTime) (game:MyGame) =
        game.GraphicsDevice.Clear Color.CornflowerBlue

    [<EntryPoint;STAThread>]
    let main argv =
        using (new Game1<Model>(init, loadContent, update, draw)) (fun game ->
            game.Content.RootDirectory <- "Content"
            game.IsMouseVisible        <- true
            game.Run()
        )
        1
