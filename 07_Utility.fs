namespace MyGame.Utility
open MyGame.Extensions
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

type String   = System.String
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

type ButtonState<'Button,'Action> =
    | IsPressed  of 'Button * 'Action
    | IsReleased of 'Button * 'Action
    | IsKeyDown  of 'Button * 'Action
    | IsKeyUp    of 'Button * 'Action

type ThumbStick<'Action> = {
    Left:  Vector2 -> 'Action
    Right: Vector2 -> 'Action
}

type Input<'Action> = {
    Keyboard:   ButtonState<Keys,   'Action> list
    GamePad:    ButtonState<Buttons,'Action> list
    ThumbStick: ThumbStick<'Action>
}

module Input =
    let mapInput definition =
        // TODO: Fix Me
        let gs = GamePad.GetState(0)

        let actions = ResizeArray<_>()

        for action in definition.Keyboard do
            match action with
            | IsPressed  (button,action) ->
                if Keyboard.isPressed  button then actions.Add action
            | IsReleased (button,action) ->
                if Keyboard.isReleased button then actions.Add action
            | IsKeyDown  (button,action) ->
                if Keyboard.isKeyDown  button then actions.Add action
            | IsKeyUp    (button,action) ->
                if Keyboard.isKeyUp    button then actions.Add action

        for action in definition.GamePad do
            match action with
            | IsKeyDown (button,action) ->
                if gs.IsButtonDown button then actions.Add action
            | IsKeyUp   (button,action) ->
                if gs.IsButtonUp   button then actions.Add action

        if gs.ThumbSticks.Left <> Vector2.Zero then
            actions.Add (definition.ThumbStick.Left  gs.ThumbSticks.Left)
        if gs.ThumbSticks.Right <> Vector2.Zero then
            actions.Add (definition.ThumbStick.Right gs.ThumbSticks.Right)

        List.ofSeq actions

type FPS = {
    mutable Updates:     int
    mutable Draws:       int
    mutable ElapsedTime: TimeSpan
    mutable UpdateFPS:   float
    mutable DrawFPS:     float
}

module FPS =
    let create updates draws time ufps dfps = {
        Updates     = updates
        Draws       = draws
        ElapsedTime = time
        UpdateFPS   = ufps
        DrawFPS     = dfps
    }

    // Global State
    let state =
        create 0 0 TimeSpan.Zero 0.0 0.0

    // Called on each update
    let update (deltaTime:TimeSpan) =
        state.Updates     <- state.Updates + 1
        state.ElapsedTime <- state.ElapsedTime + deltaTime

        if state.ElapsedTime >= TimeSpan.oneSecond then
            state.UpdateFPS   <- float state.Updates / state.ElapsedTime.TotalSeconds
            state.DrawFPS     <- float state.Draws   / state.ElapsedTime.TotalSeconds
            state.Updates     <- 0
            state.Draws       <- 0
            state.ElapsedTime <- TimeSpan.Zero

    let draw font (sb:SpriteBatch) =
        state.Draws <- state.Draws + 1
        sb.DrawString(
            spriteFont = font,
            text       = String.Format("Update/Draw: {0:0.00} {1:0.00}", state.UpdateFPS, state.DrawFPS),
            position   = Vector2(3f, 3f),
            color      = Color.Yellow,
            rotation   = 0f,
            origin     = Vector2.Zero,
            scale      = 1.33f,
            effects    = SpriteEffects.None,
            layerDepth = 0f
        )
