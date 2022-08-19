namespace myGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module GamePad =
    let isPressed state =
        state = ButtonState.Pressed

type Game1<'Model>(init, loadContent, update, draw) as this =
    inherit Game()
    let graphics            = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
    let mutable model       = Unchecked.defaultof<'Model>

    member this.Graphics = graphics

    override this.Initialize () =
        model <- init this
        base.Initialize ()

    override this.LoadContent () =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        loadContent this

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
