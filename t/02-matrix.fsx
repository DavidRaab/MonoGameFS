#!/usr/bin/env -S dotnet fsi
#r "nuget:MonoGame.Framework.DesktopGL"
#load "../01_Extensions.fs"
#load "../02_DataTypes.fs"
#load "../03_Components.fs"
#load "Test.fsx"

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Test
open MyGame
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components

// checks if two floats are nearly the same.
let nearly (x:float32) (y:float32) =
    (abs (x - y)) < 0.0001f

// checks if two vectors are nearly the same
let isVector (got:Vector2) (expected:Vector2) name =
    let (x1,y1) =    got.X,    got.Y
    let (x2,y2) = expected.X, expected.Y
    match nearly x1 x2, nearly y1 y2 with
    | true, true -> Test.pass name
    | _          ->
        Test.fail name
        printfn "# Expected (%f,%f) Given (%f,%f) " x2 y2 x1 y1

let show name (m:Matrix) =
    printfn "%s" name
    printfn "% 7.2f % 7.2f % 7.2f % 7.2f" m.M11 m.M12 m.M13 m.M14
    printfn "% 7.2f % 7.2f % 7.2f % 7.2f" m.M21 m.M22 m.M23 m.M24
    printfn "% 7.2f % 7.2f % 7.2f % 7.2f" m.M31 m.M32 m.M33 m.M34
    printfn "% 7.2f % 7.2f % 7.2f % 7.2f" m.M41 m.M42 m.M43 m.M44
    printfn ""

show "Identity" Matrix.Identity

let move = Matrix.CreateTranslation(Vector3(100f,100f,100f))
show "move" move

let rot = Matrix.CreateRotationZ(Radian.fromDeg 90f<deg> |> float32)
show "90° rot" rot

show "180° rot" (Matrix.CreateRotationZ(System.MathF.PI))
show "rotate than move" (rot * move)
show "move than rotate" (move * rot)

let vrm = Vector3.Transform(Vector3(10f,10f,1f), rot * move)
let vmr = Vector3.Transform(Vector3(10f,10f,1f), move * rot)

printfn "%A" vrm
printfn "%A" vmr