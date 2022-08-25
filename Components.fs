module MyGame.Component
open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

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
    let create pos entity =
        {Entity = entity; Position = pos }

    let entity (p:Position) =
        p.Entity

    let position (p:Position) =
        p.Position

    // State Handling
    let state = ResizeArray<Position>()

    let add pos entity =
        state.Add (create pos entity)

    let get search =
        Seq.tryFind (fun pos -> entity pos = search) state

module View =
    let create sprite e =
        {View.Entity = e; Sprite = sprite }

    let entity (v:View) =
        v.Entity

    let sprite (v:View) =
        v.Sprite

    // State Handling
    let state = ResizeArray<View>()

    let add sprite entity =
        state.Add({Entity = entity; Sprite = sprite })

    let get search =
        Seq.tryFind (fun v -> entity v = search) state


