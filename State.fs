namespace MyGame.State
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type HashSet<'a>       = System.Collections.Generic.HashSet<'a>

type State<'Component>() =
    let state            = Dictionary<Entity,'Component>()
    let set              = HashSet<Entity>()
    member this.Entities = set

    // return true if entity was not present before, otherwise false
    // this information is used to know if HashSet must be recalculated
    member this.add comp entity =
        Dictionary.add entity comp state
        this.Entities.Add entity

    member _.get entity =
        Dictionary.find entity state

    member _.map f entity =
        let mutable value = Unchecked.defaultof<_>
        if state.TryGetValue(entity, &value) then
            state.[entity] <- f value

    member _.iter f entity =
        let mutable value = Unchecked.defaultof<_>
        if state.TryGetValue(entity, &value) then
            f value

    // return true if entity was removed, otherwise false
    // this information is used to know if HashSet must be recalculated
    member this.delete entity =
        state.Remove entity |> ignore
        this.Entities.Remove entity

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

    let mutable positionsAndView = HashSet<Entity>()
    let recalcPositionsAndView () =
        positionsAndView <- HashSet.intersect State.Position.Entities State.View.Entities

    let mutable positionsAndMovement = HashSet<Entity>()
    let recalcPositionsAndMovement () =
        positionsAndMovement <- HashSet.intersect State.Position.Entities State.Movement.Entities

[<AutoOpen>]
module EntityExtension =
    type Entity with
        member this.addPosition pos =
            if State.Position.add pos this then
                Entity.recalcPositionsAndView ()
                Entity.recalcPositionsAndMovement ()

        member this.addView view =
            if State.View.add view this then
                Entity.recalcPositionsAndView ()

        member this.addMovement   mov =
            if State.Movement.add mov this then
                Entity.recalcPositionsAndMovement ()

        member this.deletePosition () =
            if State.Position.delete this then
                Entity.recalcPositionsAndView ()
                Entity.recalcPositionsAndMovement ()

        member this.deleteView () =
            if State.View.delete this then
                Entity.recalcPositionsAndView ()

        member this.deleteMovement () =
            if State.Movement.delete this then
                Entity.recalcPositionsAndMovement ()

