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


