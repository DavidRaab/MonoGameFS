namespace MyGame.Entity
open MyGame
open MyGame.DataTypes
open MyGame.State
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

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
