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

    let update pos e =
        match Seq.tryFindIndex (fun p -> entity p = e) state with
        | None     -> add pos e
        | Some idx -> state.[idx] <- create pos e


module View =
    let create sprite e =
        {View.Entity = e; Sprite = sprite }

    let entity (v:View) = v.Entity
    let sprite (v:View) = v.Sprite

    // State Handling
    let state = ResizeArray<View>()

    let add sprite e =
        state.Add (create sprite e)

    let get search =
        Seq.tryFind (fun v -> entity v = search) state

module Movement =
    let create dir e =
        { Entity = e; Direction = dir }

    let entity    (m:Movement) = m.Entity
    let direction (m:Movement) = m.Direction

    // State Handling
    let mutable state = ResizeArray<Movement>()

    let add dir e =
        state.Add (create dir e)

    let get search =
        Seq.tryFind (fun m -> entity m = search) state

    let delete search =
        let newA = ResizeArray<Movement>()
        for s in state do
            if s.Entity <> search then
                newA.Add s
        state <- newA