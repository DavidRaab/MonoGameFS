namespace MyGame.State
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type HashSet<'a>       = System.Collections.Generic.HashSet<'a>

type State<'Component>() =
    let addedDeleted              = Event<unit>()
    let state                     = Dictionary<Entity,'Component>()
    let set                       = HashSet<Entity>()
    member this.Entities          = set
    member this.OnEntitiesChanged = addedDeleted.Publish

    member this.add comp entity =
        Dictionary.add entity comp state
        if this.Entities.Add entity then
            addedDeleted.Trigger ()

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

    member this.delete entity =
        state.Remove entity |> ignore
        if this.Entities.Remove entity then
            addedDeleted.Trigger ()

module State =
    let Position = State<Position>()
    let View     = State<View>()
    let Movement = State<Movement>()


