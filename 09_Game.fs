namespace MyGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type MonoGame<'Assets,'GameState>(init, loadAssets, initModel, update, draw) as this =
    inherit Game()
    let graphics       = new GraphicsDeviceManager(this)
    let mutable sb     = Unchecked.defaultof<SpriteBatch>
    let mutable assets = Unchecked.defaultof<'Assets>
    let mutable model  = Unchecked.defaultof<'GameState>

    member this.Graphics    = graphics
    member this.Asset       = assets
    member this.SpriteBatch = sb
    member this.Gd          = this.GraphicsDevice

    override this.Initialize () =
        init this
        base.Initialize ()

    override this.LoadContent () =
        sb     <- new SpriteBatch(this.GraphicsDevice)
        assets <- loadAssets this
        model  <- initModel assets
        base.LoadContent ()

    override this.Update(gameTime) =
        model <- update model gameTime this
        base.Update gameTime

    override this.Draw gameTime =
        draw model gameTime this
        base.Draw gameTime

    member this.SetResolution x y =
        graphics.PreferredBackBufferWidth  <- x
        graphics.PreferredBackBufferHeight <- y
        graphics.ApplyChanges ()

    member this.GetResolution () =
        graphics.PreferredBackBufferWidth, graphics.PreferredBackBufferHeight
