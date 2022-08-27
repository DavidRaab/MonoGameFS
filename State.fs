namespace MyGame.State
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>

type State<'Component>() =
    let state = Dictionary<Entity,'Component>()

    member _.add comp entity =
        Dictionary.add entity comp state

    member _.get entity =
        Dictionary.find entity state

    member _.delete entity =
        state.Remove entity |> ignore

module State =
    let Position = State<Position>()
    let View     = State<View>()
    let Movement = State<Movement>()

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

    let addPosition = State.Position.add
    let addView     = State.View.add
    let addMovement = State.Movement.add

    let deletePosition = State.Position.delete
    let deleteView     = State.View.delete
    let deleteMovement = State.Movement.delete

[<AutoOpen>]
module EntityExtension =
    type Entity with
        member this.addPosition pos = Entity.addPosition pos this
        member this.addView    view = Entity.addView    view this
        member this.addMovement mov = Entity.addMovement mov this
        member this.deletePosition () = Entity.deletePosition this
        member this.deleteView     () = Entity.deleteView     this
        member this.deleteMovement () = Entity.deleteMovement this
