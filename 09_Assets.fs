namespace MyGame.Assets
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Assets = {
    Font:    Fonts
    Sprites: Sprites
    Knight:  SheetAnimations
}
and Sprites = {
    Pixel:    Sprite
    Missing:  Sprite
    Arrow:    Sprite
    WhiteBox: Sprite
}
and Fonts = {
    Default: SpriteFont
}

module Assets =
    let load load loadFont texture = {
        Font = {
            Default = loadFont "Font"
        }
        Sprites = {
            Pixel    = Sprite.fromTexture (texture  1  1 [|Color.White|])
            Missing  = Sprite.fromTexture (texture  1  1 [|Color.Pink|])
            WhiteBox = Sprite.fromTexture (texture 10 10 (Array.replicate 100 Color.White))
            Arrow    = Sprite.fromTexture (load "arrow")
        }
        Knight = SheetAnimations.create "Idle" [
            "Idle"   => SheetAnimation.create 100<ms> true  (Sheet.fromColumnsRows 10 1 (load "FreeKnight/Idle"))
            "Attack" => SheetAnimation.create  50<ms> false (Sheet.fromColumnsRows  4 1 (load "FreeKnight/Attack"))
            "Run"    => SheetAnimation.create 100<ms> true  (Sheet.fromColumnsRows 10 1 (load "FreeKnight/Run"))
            "Crouch" => SheetAnimation.create   0<ms> false (Sheet.fromTexture          (load "FreeKnight/Crouch"))
        ]
    }