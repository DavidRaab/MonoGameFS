namespace MyGame.Entity
open MyGame
open MyGame.DataTypes
open MyGame.State
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

// Provides an Cache for the State<'a> classes
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

    let all () =
        entities :> seq<Entity>

    let positionsAndView = EntitiesCache([
            Cache.create State.Position
            Cache.create State.View
        ])

    let positionsAndMovement = EntitiesCache([
            Cache.create State.Position
            Cache.create State.Movement
        ])

[<AutoOpen>]
module EntityExtension =
    type Entity with
        member this.addPosition pos   = State.Position.add pos this
        member this.addView view      = State.View.add view this
        member this.addMovement   mov = State.Movement.add mov this
        member this.deletePosition () = State.Position.delete this
        member this.deleteView ()     = State.View.delete this
        member this.deleteMovement () = State.Movement.delete this

