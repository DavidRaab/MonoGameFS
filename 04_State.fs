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
    member this.Data              = state
    member this.OnEntitiesChanged = onEntitiesChanged.Publish

    /// adds or overwrite 'Component for Entity
    member this.add comp entity =
        Dictionary.add entity comp state |> ignore
        if this.Entities.Add entity then
            onEntitiesChanged.Trigger ()

    /// Get 'Component for Entity
    member _.get entity =
        Dictionary.find entity state

    /// Reads the current 'Component from state and lets you return a new 'Component.
    /// Typically only useful when the computation depends on the current value or
    /// a 'Component should be Created/Removed during computation.
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

    /// try to read current 'Component of Entity and executes mapping function
    /// when a component is defined. mapping function then returns the new
    /// component that is used to overwrite previous one.
    member _.map f entity =
        match state.TryGetValue entity with
        | true, value -> state.[entity] <- f value
        | false, _    -> ()

    /// try to read current 'Component of Entity and when exists passes it to the function.
    /// As the function returns nothing this method is useful when you want
    /// to change a mutable field of a 'Component or do other kind of side-effects.
    member _.iter f entity =
        match state.TryGetValue entity with
        | true, value -> f value
        | false, _    -> ()

    /// Deletes 'Component for Entity
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
