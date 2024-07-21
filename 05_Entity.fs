namespace MyGame.Entity
open MyGame
open MyGame.DataTypes
open MyGame.Components
open MyGame.State
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// Provides a Cache for getting entities with certain components
type ICacheable =
    abstract member Entities          : HashSet<Entity>
    abstract member OnEntitiesChanged : IEvent<unit>

type Cache =
    static member inline create x = {
        new ICacheable with
            member this.Entities          = (^Cache : (member Entities          : HashSet<Entity>) x)
            member this.OnEntitiesChanged = (^Cache : (member OnEntitiesChanged : IEvent<unit>   ) x)
    }

type EntitiesCache<'a when 'a :> ICacheable>(states:seq<'a>) =
    let outdated      = Array.replicate (Seq.length states) true
    let entities      = Array.ofSeq     (states |> Seq.map (fun state -> state.Entities))
    let mutable cache = HashSet<Entity>()

    do
        states |> Seq.iteri (fun idx state ->
            state.OnEntitiesChanged |> Event.add (fun () ->
                outdated.[idx] <- true
            )
        )

    member _.GetCache () =
        if Array.exists id outdated then
            cache <- HashSet.intersectMany entities
            for idx=0 to outdated.Length-1 do
                outdated.[idx] <- false
            cache
        else
            cache

module Entity =
    let mutable private counter = 0
    let private entities = ResizeArray<Entity>()

    let create () =
        counter <- counter + 1
        let e = Entity counter
        entities.Add e
        e

    let init f =
        let entity = create ()
        f entity
        entity

    let initMany count f = [
        for idx in 0 .. count-1 do
            let e = create ()
            f idx e
            yield e
    ]

    let all () =
        entities :> seq<Entity>

    // let transformAndView = EntitiesCache([
    //     Cache.create State.Transform
    //     Cache.create State.View
    // ])

    let transformAndMovement = EntitiesCache([
        Cache.create State.Transform
        Cache.create State.Movement
    ])

[<AutoOpen>]
module EntityExtension =
    type Entity with
        member entity.addTransform t     = State.Transform.add t entity
        member entity.deleteTransform () = State.Transform.delete entity
        member entity.addView view       = State.View.add entity true view
        member entity.deleteView ()      = State.View.remove entity
        member entity.addMovement mov    = State.Movement.add mov entity
        member entity.deleteMovement ()  = State.Movement.delete entity
        member entity.addAnimation anim  = State.Animation.add anim entity
        member entity.deleteAnimation () = State.Animation.delete entity
        member entity.setAnimation name =
            entity |> State.Animation.iter (fun anim ->
                Animation.switchAnimation name anim
            )
        member entity.getSheetExn name : Sheet =
            match State.Animation.get entity with
            | ValueSome anim -> anim.Sheets.Sheets.[name]
            | ValueNone      -> failwithf "%A has no SheetAnimations" entity

