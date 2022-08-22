module MyGame.App
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input


type Fonts = {
    Default: SpriteFont
}

type Textures = {
    Background: Texture2D
    Pixel:      Texture2D
    WhiteBox:   Texture2D
}

type Assets = {
    Font:    Fonts
    Texture: Textures
    Rects:   ResizeArray<RectangleOO>
}

type GameState = GameState

module GameState =
    let create () = GameState

type MyGame = Game1<Assets,GameState>

let init (game:MyGame) =
    game.SetResolution 854 480
    GameState.create ()


let loadContent (game:MyGame) =
    let rects = ResizeArray<_>()
    let yOffset = 0f
    for x=1 to 75 do
        for y=1 to 40 do
            rects.Add (RectangleOO (Vector2 (float32 x * 11f, float32 y * 11f + yOffset), Color.White))

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
        Rects = rects
    }

    Rectangles.loadContent ()
    Collision.loadContent assets.Texture.Pixel

    assets


let update (model:GameState) (gameTime:GameTime) (game:MyGame) =
    FPS.update gameTime
    Rectangles.update gameTime
    Collision.update gameTime

    let gamePad  = GamePad.GetState PlayerIndex.One
    let keyboard = Keyboard.GetState ()

    // for rect in game.Asset.Rects do
    //     rect.Update()

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

    model


let draw (model:GameState) (gameTime:GameTime) (game:MyGame) =
    game.GraphicsDevice.Clear(Color.CornflowerBlue)
    let width, height = game.GetResolution ()

    game.spriteBatch.Draw(
        game.Asset.Texture.Background,
        Rectangle(0, 0, width, height),
        Color.White
    )

    Rectangles.draw game.Asset.Texture.Pixel game.spriteBatch

    // for rect in game.Asset.Rects do
    //     rect.Draw(game.Asset.Texture.Background, game.spriteBatch)

    FPS.draw game.Asset.Font.Default game.spriteBatch

    for draw in Collision.draw game.Asset.Texture.Pixel do
        match draw with
        | Collision.Draw (tex,rect,color) ->
            game.spriteBatch.Draw(tex,rect,color)

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


// Run MonoGame Application
[<EntryPoint;STAThread>]
let main argv =
    using (new Game1<Assets,GameState>(init, loadContent, update, draw)) (fun game ->
        game.Graphics.SynchronizeWithVerticalRetrace <- true
        game.IsFixedTimeStep       <- true
        game.TargetElapsedTime     <- TimeSpan.FromSeconds(1.0 / 60.0)
        game.Content.RootDirectory <- "Content"
        game.IsMouseVisible        <- true
        game.Run()
    )
    1
