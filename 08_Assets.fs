namespace MyGame.Assets
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Assets = {
    Font:    Fonts
    Texture: Textures
    Knight:  SheetAnimations
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
        Knight = SheetAnimations.create "Idle" [
            "Idle",   SheetAnimation.create 100 true (Sheet.fromColumnsRows 10 1 (load "FreeKnight/Idle"))
            "Attack", SheetAnimation.create  50 true (Sheet.fromColumnsRows  4 1 (load "FreeKnight/Attack"))
            "Run",    SheetAnimation.create 100 true (Sheet.fromColumnsRows 10 1 (load "FreeKnight/Run"))
            "Crouch", SheetAnimation.create   0 true (Sheet.fromTexture          (load "FreeKnight/Crouch"))
        ]
    }