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

    /// adds or overwrite 'Component for Entity
    member this.add comp entity =
        Dictionary.add entity comp state |> ignore
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
        match state.TryGetValue entity with
        | true, value -> state.[entity] <- f value
        | false, _    -> ()

    member _.iter f entity =
        match state.TryGetValue entity with
        | true, value -> f value
        | false, _    -> ()

    member this.delete entity =
        state.Remove entity |> ignore
        if this.Entities.Remove entity then
            onEntitiesChanged.Trigger ()

module State =
    let mutable camera   = Unchecked.defaultof<Camera>
    let mutable uiCamera = Unchecked.defaultof<Camera>
    let Transform        = State<Transform>()
    let View             = State<View>()
    let Movement         = State<Movement>()
    let SheetAnimations  = State<SheetAnimations>()
