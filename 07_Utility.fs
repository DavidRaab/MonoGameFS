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
