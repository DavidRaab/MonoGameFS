namespace MyGame.Assets
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Assets = {
    Font:    Fonts
    Texture: Textures
    Knight:  Knight
}
and Knight = {
    Attack: Sheet
    Idle:   Sheet
    Run:    Sheet
    Crouch: Sheet
}
and Textures = {
    Missing:  Texture2D
    Arrow:    Texture2D
    Pixel:    Texture2D
    WhiteBox: Texture2D
}
and Fonts = {
    Default: SpriteFont
}

module Assets =
    let load load loadFont texture = {
        Font = {
            Default = loadFont "Font"
        }
        Texture = {
            Missing  = texture 1  1 [|Color.Pink|]
            Arrow    = load "arrow"
            WhiteBox = texture 10 10 (Array.replicate 100 Color.White)
            Pixel    = texture 1  1 [|Color.White|]
        }
        Knight = {
            Attack = Sheet.fromColumnsRows (load "FreeKnight/Attack") 4 1
            Idle   = Sheet.fromColumnsRows (load "FreeKnight/Idle")  10 1
            Run    = Sheet.fromColumnsRows (load "FreeKnight/Run")   10 1
            Crouch = Sheet.fromTexture     (load "FreeKnight/Crouch")
        }
    }