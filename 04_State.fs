namespace MyGame.State
open MyGame
open MyGame.DataTypes

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type HashSet<'a>       = System.Collections.Generic.HashSet<'a>

type State<'Component>() =
    let onEntitiesChanged         = Event<unit>()
    let state                     = Dictionary<Entity,'Component>()
    let set                       = HashSet<Entity>()
    member this.Entities          = set
    member this.OnEntitiesChanged = onEntitiesChanged.Publish

    member this.add comp entity =
        Dictionary.add entity comp state
        if this.Entities.Add entity then
            onEntitiesChanged.Trigger ()

    member _.get entity =
        Dictionary.find entity state

    member this.change entity f =
        let before = state.ContainsKey entity
        Dictionary.change entity f state
        let after  = state.ContainsKey entity
        match before,after with
        | true, false ->
            this.Entities.Remove entity |> ignore
            onEntitiesChanged.Trigger ()
        | false, true ->
            this.Entities.Add entity |> ignore
            onEntitiesChanged.Trigger ()
        | false, false | true, true ->
            ()

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
            onEntitiesChanged.Trigger ()

module State =
    let mutable camera       = Unchecked.defaultof<Camera>
    let mutable cameraScreen = Unchecked.defaultof<Camera>
    let Position             = State<Position>()
    let View                 = State<View>()
    let Movement             = State<Movement>()
    let SheetAnimations      = State<SheetAnimations>()
