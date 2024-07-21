namespace MyGame.Assets
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Assets = {
    Font:    Fonts
    Sprites: Sprites
    Knight:  Sheets
    Box:     Sheets
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
    let inline ms time : TimeSpan =
        TimeSpan.FromMilliseconds(time)

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
        Knight = Sheets.create {
            Default = "Idle"
            Sheets  = Map [
                "Idle"   => Sheet.create { FrameDuration = (ms 100); IsLoop =  true; Sprites = (Sprite.fromColumnsRows 10 1 (load "FreeKnight/Idle"))   }
                "Attack" => Sheet.create { FrameDuration =  (ms 50); IsLoop = false; Sprites = (Sprite.fromColumnsRows  4 1 (load "FreeKnight/Attack")) }
                "Run"    => Sheet.create { FrameDuration = (ms 100); IsLoop =  true; Sprites = (Sprite.fromColumnsRows 10 1 (load "FreeKnight/Run"))    }
                "Crouch" => Sheet.create { FrameDuration =   (ms 0); IsLoop = false; Sprites = [| Sprite.fromTexture        (load "FreeKnight/Crouch") |] }
            ]
        }
        Box = Sheets.create {
            Default = "Default"
            Sheets  = Map [
                "Default" =>
                    Sheet.create { FrameDuration = (ms 250); IsLoop = true; Sprites = [|
                        Sprite.fromTexture (texture 10 10 (Array.replicate 100 Color.White))
                        Sprite.fromTexture (texture 10 10 (Array.replicate 100 Color.Red))
                        Sprite.fromTexture (texture 10 10 (Array.replicate 100 Color.Blue))
                    |]
                }
            ]
        }
    }