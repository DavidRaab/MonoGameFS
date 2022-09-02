namespace MyGame.Utility
open MyGame.Extensions
open Microsoft.Xna.Framework.Input

type TimeSpan = System.TimeSpan

type KeyboardState() =
    let state = System.Collections.BitArray(256)
    member this.SetKey (key:Keys) =
        state.Set(int key, true)
    member this.GetKey (key:Keys) =
        state.Get(int key)
    member this.SetKeys(keys:Keys[]) =
        for key in keys do
            state.Set(int key, true)
    member this.IsKeyDown(key:Keys) =
        state.Get(int key) = true
    member this.IsKeyUp(key:Keys) =
        state.Get(int key) = false

module Keyboard =
    let mutable previousState = KeyboardState()
    let mutable currentState  = KeyboardState()

    let addKeys(keys) =
        currentState.SetKeys keys

    let nextState () =
        previousState <- currentState
        currentState  <- KeyboardState()

    // returns true only in the first frame the key was pressed.
    // Key must be released again to become true again
    let isPressed key =
        previousState.IsKeyUp key && currentState.IsKeyDown key

    // returns true only in the first frame the key was released
    // Key must be pressed again to become true again
    let isReleased key =
        previousState.IsKeyDown key && currentState.IsKeyUp key

    let isKeyDown key =
        currentState.GetKey key

    let isKeyUp key =
        currentState.IsKeyUp key

// TODO: What i need later -- but maybe this gets into an Animation system
// Currently only provides the 3. form
// 1. Run once after time
// 2. Run for a duration
// 3. Run periodically after a time

module Timed =
    let fixedUpdateTiming =
        TimeSpan.FromSeconds(1.0 / 60.0)

    // Do something after a specific time is elapsed
    let runEveryTimeFrame timeFrame =
        let mutable elapsedTime = TimeSpan.Zero
        fun f deltaTime->
            elapsedTime <- elapsedTime + deltaTime
            if elapsedTime >= timeFrame then
                f ()
                elapsedTime <- elapsedTime - timeFrame

    let runWithState state =
        let mutable state = state
        fun f ->
            state <- f state

    let runGC =
        runEveryTimeFrame TimeSpan.oneSecond (fun () ->
            System.GC.Collect ()
        )

    let runFixedUpdateTiming =
        runEveryTimeFrame fixedUpdateTiming