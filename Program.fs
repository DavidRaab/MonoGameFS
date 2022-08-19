namespace myGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module App =
    let init () =
        ()

    let loadContent () =
        ()

    let update (game:Game1) gameTime =
        let gamePad  = GamePad.GetState PlayerIndex.One
        let keyboard = Keyboard.GetState ()

        if GamePad.isPressed gamePad.Buttons.Back || keyboard.IsKeyDown Keys.Escape then
            game.Exit()

    let draw (game:Game1) gameTime =
        game.GraphicsDevice.Clear Color.CornflowerBlue

    [<EntryPoint;System.STAThread>]
    let main argv =
        using (new Game1(init, loadContent, update, draw)) (fun game ->
            game.Run()
        )
        1
