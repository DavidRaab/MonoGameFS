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
        member this.addTransform t           = State.Transform.add t this
        member this.deleteTransform ()       = State.Transform.delete this
        member entity.addView view           = State.View.add entity true view
        member this.deleteView ()            = State.View.remove this
        member this.addMovement mov          = State.Movement.add mov this
        member this.deleteMovement ()        = State.Movement.delete this
        member this.addSheetAnimations anim  = State.SheetAnimations.add anim this
        member this.deleteSheetAnimations () = State.SheetAnimations.delete this
        member this.setAnimation name =
            this |> State.SheetAnimations.iter (fun anims ->
                SheetAnimations.setAnimation name anims
            )
        member this.getAnimationExn name =
            match State.SheetAnimations.get this with
            | ValueSome anims -> SheetAnimations.getAnimationExn name anims
            | ValueNone       -> failwithf "%A has no SheetAnimations" this

