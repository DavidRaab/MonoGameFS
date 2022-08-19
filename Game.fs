namespace myGame
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

module GamePad =
    let isPressed state =
        state = ButtonState.Pressed

type Game1(init, loadContent, update, draw) as this =
    inherit Game()
    let _graphics            = new GraphicsDeviceManager(this)
    let mutable _spriteBatch = Unchecked.defaultof<SpriteBatch>
    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- true

    override this.Initialize () =
        init ()
        base.Initialize ()

    override this.LoadContent () =
        _spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        loadContent ()

    override this.Update(gameTime) =
        update this gameTime
        base.Update gameTime

    override this.Draw gameTime =
        draw this gameTime
        base.Draw gameTime
