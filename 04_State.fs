namespace MyGame.State
open MyGame
open MyGame.DataTypes
open Storage

type Dictionary<'a,'b> = System.Collections.Generic.Dictionary<'a,'b>
type HashSet<'a>       = System.Collections.Generic.HashSet<'a>

type State<'Component>() =
    let state                     = Dictionary<Entity,'Component>()
    let set                       = HashSet<Entity>()
    member this.Entities          = set
    member this.Data              = state

    /// adds or overwrite 'Component for Entity
    member this.add comp entity : unit =
        ignore (Dictionary.add entity comp state)
        ignore (this.Entities.Add entity        )

    /// Get 'Component for Entity
    member _.get entity : 'Component voption =
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
        | false, true ->
            this.Entities.Add entity |> ignore
        | false, false | true, true ->
            ()

    /// try to read current 'Component of Entity and executes mapping function
    /// when a component is defined. mapping function then returns the new
    /// component that is used to overwrite previous one.
    member _.fetchReplace f entity =
        match state.TryGetValue entity with
        | true, value -> state.[entity] <- f value
        | false, _    -> ()

    /// try to read current 'Component of Entity and when exists passes it to the function.
    /// As the function returns nothing this method is useful when you want
    /// to change a mutable field of a 'Component or do other kind of side-effects.
    member _.fetch f entity =
        match state.TryGetValue entity with
        | true, value -> f value
        | false, _    -> ()

    /// Deletes 'Component for Entity
    member this.delete entity : unit =
        ignore (state.Remove entity        )
        ignore (this.Entities.Remove entity)


module State =
    let mutable camera   = Unchecked.defaultof<Camera>
    let mutable uiCamera = Unchecked.defaultof<Camera>
    let Transform : Storage<Entity,Transform> = Storage.create ()
    let Movement         = State<Movement>()
    let Animation        = State<Animation>()

    // let View          = State<View>()
    module View =
        let visible = Storage.create () // Dictionary<Entity,View>()
        let hidden  = Storage.create () // Dictionary<Entity,View>()

        /// Removes the View for an Entity
        let inline remove (e:Entity) : unit =
            Storage.remove e visible
            Storage.remove e hidden

        /// Adds a View for an Entity
        let add (e:Entity) (isVisible:bool) (v:View) : unit =
            if isVisible then
                Storage.remove e hidden
                Storage.add    e v visible
            else
                Storage.remove e visible
                Storage.add    e v hidden

        /// Get visibility and View of an Entity. If no View is present returns ValueNone
        let get (e:Entity) : (bool * View) voption =
            match Storage.get e visible with
            | ValueSome view -> ValueSome (true,view)
            | ValueNone      ->
                match Storage.get e hidden with
                | ValueSome view -> ValueSome (false,view)
                | ValueNone      -> ValueNone

        /// Returns if Entity is Visible. Also returns false when no View was added for an Entity.
        let inline isVisible (e:Entity) =
            Storage.contains e visible

        /// Sets Visibility. Does nothing when no View was added for an Entity.
        let inline setVisiblity (e:Entity) (should_be_visible:bool) =
            match get e with
            | ValueSome(_,view) -> add e should_be_visible view
            | ValueNone         -> ()

        /// Switches visibility. Does nothing when no View was added for an Entity.
        let inline switchVisibility (e:Entity) =
            match get e with
            | ValueSome(true,view) ->
                Storage.remove e visible
                Storage.add e view hidden
            | ValueSome(false,view) ->
                Storage.remove e hidden
                Storage.add e view visible
            | ValueNone -> ()

        /// Get View, runs function on it, and stores the new View returned by it
        let inline map ([<InlineIfLambda>] f: View -> View) (e:Entity) =
            match get e with
            | ValueSome(true, view) -> Storage.add e (f view) visible
            | ValueSome(false,view) -> Storage.add e (f view) hidden
            | ValueNone             -> ()

        /// Get view and runs a function on it. Does nothing when no View was added for an Entity
        let inline iter ([<InlineIfLambda>] f: View -> unit) (e:Entity) =
            match get e with
            | ValueSome(_,view) -> f view
            | ValueNone         -> ()
