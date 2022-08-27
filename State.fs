namespace MyGame.State
open MyGame
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

module Entity =
    let mutable private counter = 0
    let private entities = ResizeArray<Entity>()

    let create () =
        counter <- counter + 1
        let e = Entity counter
        entities.Add e
        e

    let all () =
        entities :> seq<Entity>

module Position =
    let state = Dictionary<Entity,Position>()

    let add pos entity =
        Dictionary.add entity (Position.create pos) state

    let get search =
        Dictionary.find search state

module View =
    let state = Dictionary<Entity,View>()

    let add sprite entity =
        Dictionary.add entity (View.create sprite) state

    let get search =
        Dictionary.find search state

module Movement =
    let mutable state = Dictionary<Entity,Movement>()

    let add dir entity =
        Dictionary.add entity (Movement.create dir) state

    let get entity =
        Dictionary.find entity state

    let delete entity =
        state.Remove(entity) |> ignore