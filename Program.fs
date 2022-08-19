namespace myGame
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type FPS = {
    Frames:      int
    ElapsedTime: TimeSpan
    FPS:         float
}

module FPS =
    let create frames time fps = {
        Frames      = frames
        ElapsedTime = time
        FPS         = fps
    }

    let empty =
        create 0 (TimeSpan 0) 0.0

    let increment time fps =
        create (fps.Frames + 1) (fps.ElapsedTime + time) fps.FPS

    let calculateFps fps =
        if fps.ElapsedTime.TotalSeconds >= 1.0 then
            create 0 (TimeSpan 0) (float fps.Frames / fps.ElapsedTime.TotalSeconds)
        else
            fps

    let updateFps time fps =
        calculateFps (increment time fps)

type Model = {
    FPS: FPS
}

module Model =
    let create fps = {
        FPS = fps
    }

    let updateFps (gameTime:GameTime) model = {
        model with Model.FPS = FPS.updateFps gameTime.ElapsedGameTime model.FPS
    }

type MyGame = Game1<Model>



module App =
    let init (game:MyGame) =
        game.SetResolution 800 600
        Model.create FPS.empty

    let loadContent (game:MyGame) =
        ()

    let update (model:Model) (gameTime:GameTime) (game:MyGame) =
        let model = Model.updateFps gameTime model
        printfn "Model: %A" model

        let gamePad  = GamePad.GetState PlayerIndex.One
        let keyboard = Keyboard.GetState ()

        if GamePad.isPressed gamePad.Buttons.Back || keyboard.IsKeyDown Keys.Escape then
            game.Exit()

        model

    let draw (model:Model) (gameTime:GameTime) (game:MyGame) =
        game.GraphicsDevice.Clear Color.CornflowerBlue

    [<EntryPoint;System.STAThread>]
    let main argv =
        using (new Game1<Model>(init, loadContent, update, draw)) (fun game ->
            game.Content.RootDirectory <- "Content"
            game.IsMouseVisible        <- true
            game.Run()
        )
        1
