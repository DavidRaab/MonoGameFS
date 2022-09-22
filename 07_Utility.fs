namespace MyGame.Utility
open MyGame.Extensions
open MyGame.DataTypes
open MyGame.Components
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

type String   = System.String
type TimeSpan = System.TimeSpan

type FKeyboardState() =
    let state = System.Collections.BitArray(256)
    member this.SetKey (key:Input.Keys) =
        state.Set(int key, true)
    member this.GetKey (key:Input.Keys) =
        state.Get(int key)
    member this.SetKeys(keys:Input.Keys array) =
        for key in keys do
            state.Set(int key, true)
    member this.IsKeyDown(key:Input.Keys) =
        state.Get(int key) = true
    member this.IsKeyUp(key:Input.Keys) =
        state.Get(int key) = false

module FKeyboard =
    let mutable previousState = FKeyboardState()
    let mutable currentState  = FKeyboardState()

    let addKeys keys =
        currentState.SetKeys keys

    let nextState () =
        previousState <- currentState
        currentState  <- FKeyboardState()

    /// returns true only in the first frame the key was pressed.
    /// Key must be released again to become true again
    let isPressed key =
        previousState.IsKeyUp key && currentState.IsKeyDown key

    /// returns true only in the first frame the key was released
    /// Key must be pressed again to become true again
    let isReleased key =
        previousState.IsKeyDown key && currentState.IsKeyUp key

    let isKeyDown key =
        currentState.GetKey key

    let isKeyUp key =
        currentState.IsKeyUp key

type FGamePadButton =
    | A             = 0
    | B             = 1
    | X             = 2
    | Y             = 3
    | LeftShoulder  = 4
    | RightShoulder = 5
    | LeftStick     = 6
    | RightStick    = 7
    | DPadLeft      = 8
    | DPadUp        = 9
    | DPadRight     = 10
    | DPadDown      = 11
    | Back          = 12
    | BigButton     = 13
    | Start         = 14

type FGamePadState() =
    let state                = System.Collections.BitArray(15)
    let mutable triggerLeft  = 0f
    let mutable triggerRight = 0f
    let mutable stickLeft    = Vector2.Zero
    let mutable stickRight   = Vector2.Zero

    member this.SetButton (button:FGamePadButton) =
        state.Set(int button, true)
    member this.GetButton (button:FGamePadButton) =
        state.Get(int button)
    member this.AddGamePadState (gs:Input.GamePadState) =
        // Map MonoGame Buttons to own State
        let b = gs.Buttons
        let buttonMapping buttons =
            for (mb,fb) in buttons do
                if mb = Input.ButtonState.Pressed then this.SetButton(fb)
        buttonMapping [
            b.A,             FGamePadButton.A
            b.B,             FGamePadButton.B
            b.X,             FGamePadButton.X
            b.Y,             FGamePadButton.Y
            b.LeftShoulder,  FGamePadButton.LeftShoulder
            b.RightShoulder, FGamePadButton.RightShoulder
            b.LeftStick,     FGamePadButton.LeftStick
            b.RightStick,    FGamePadButton.RightStick
            b.Back,          FGamePadButton.Back
            b.BigButton,     FGamePadButton.BigButton
            b.Start,         FGamePadButton.Start
        ]

        // Map MonoGame DPad State to own State
        let d = gs.DPad
        let dpadMapping dpads =
            for (md,fd) in dpads do
                if md = Input.ButtonState.Pressed then this.SetButton(fd)
        dpadMapping [
            d.Left,  FGamePadButton.DPadLeft
            d.Up,    FGamePadButton.DPadUp
            d.Right, FGamePadButton.DPadRight
            d.Down,  FGamePadButton.DPadDown
        ]

        // Keep Track of the highest value
        triggerLeft  <- max triggerLeft  gs.Triggers.Left
        triggerRight <- max triggerRight gs.Triggers.Right

        // Save last Thumbstick values
        stickLeft  <- gs.ThumbSticks.Left
        stickRight <- gs.ThumbSticks.Right

    member this.TriggerLeft  = triggerLeft
    member this.TriggerRight = triggerRight
    member this.StickLeft    = stickLeft
    member this.StickRight   = stickRight

    member this.IsKeyUp button =
        state.Get(int button) = false
    member this.IsKeyDown button =
        state.Get(int button) = true

module FGamePad =
    let mutable previousState = FGamePadState()
    let mutable currentState  = FGamePadState()

    let addState gamePadState =
        currentState.AddGamePadState gamePadState

    let nextState () =
        previousState <- currentState
        currentState  <- FGamePadState()

    /// `true` only in the exact frame this button is pressed
    let isPressed button =
        previousState.IsKeyUp button && currentState.IsKeyDown button

    /// `true` only in the frame the button is released
    let isReleased button =
        previousState.IsKeyDown button && currentState.IsKeyUp button

    let isKeyDown button =
        currentState.IsKeyDown button

    let isKeyUp button =
        currentState.IsKeyUp button

    let stickLeft    () = currentState.StickLeft
    let stickRight   () = currentState.StickRight
    let triggerLeft  () = currentState.TriggerLeft
    let triggerRight () = currentState.TriggerRight

type MouseButton =
    | Left     = 0
    | Middle   = 1
    | Right    = 2
    | XButton1 = 3
    | XButton2 = 4

type FMouseState() =
    let state                         = System.Collections.BitArray(5)
    let mutable position              = Point(0,0)
    let mutable scrollWheel           = 0
    let mutable horizontalScrollWheel = 0

    member this.SetButton(button:MouseButton) =
        state.Set(int button,true)
    member this.GetButton(button:MouseButton) =
        state.Get(int button)
    member this.AddMouseState(ms:Input.MouseState) =
        // Map Mouse Button o BitArray
        let mapButtons buttons =
            for (mb,fb) in buttons do
                if mb = Input.ButtonState.Pressed then
                    this.SetButton(fb)
        mapButtons [
            ms.LeftButton,   MouseButton.Left
            ms.MiddleButton, MouseButton.Middle
            ms.RightButton,  MouseButton.Right
            ms.XButton1,     MouseButton.XButton1
            ms.XButton2,     MouseButton.XButton2
        ]
        position              <- Point(ms.X, ms.Y)
        scrollWheel           <- scrollWheel  + ms.ScrollWheelValue
        horizontalScrollWheel <- horizontalScrollWheel + ms.HorizontalScrollWheelValue

    member this.Position              = position
    member this.ScrollWheel           = scrollWheel
    member this.HorizontalScrollWheel = horizontalScrollWheel

    member this.IsKeyDown button =
        this.GetButton(button) = true
    member this.IsKeyUp button =
        this.GetButton(button) = false

module FMouse =
    let mutable previousState = FMouseState()
    let mutable currentState  = FMouseState()

    let addState mouseState =
        currentState.AddMouseState mouseState

    let nextState () =
        previousState <- currentState
        currentState  <- FMouseState ()

    /// `true` only in the exact frame the button was pressed
    let isPressed button =
        previousState.IsKeyUp button && currentState.IsKeyDown button

    /// `true` only in the exact frame the button is released
    let isReleased button =
        previousState.IsKeyDown button && currentState.IsKeyUp button

    let isKeyDown button =
        currentState.IsKeyDown button

    let isKeyUp button =
        currentState.IsKeyUp button

    let position              () = currentState.Position
    let scrollWheel           () = currentState.ScrollWheel
    let horizontalScrollWheel () = currentState.HorizontalScrollWheel

// Input Module
type ButtonState =
    | IsPressed
    | IsReleased
    | IsKeyDown
    | IsKeyUp

type GamePadThumbStick<'Action> = {
    Left:  option<Vector2 -> 'Action>
    Right: option<Vector2 -> 'Action>
}

type GamePadTriggers<'Action> = {
    Left:  option<float32 -> 'Action>
    Right: option<float32 -> 'Action>
}

type InputGamePad<'Action> = {
    Buttons:    list<FGamePadButton * ButtonState * 'Action>
    ThumbStick: GamePadThumbStick<'Action>
    Trigger:    GamePadTriggers<'Action>
}

type FMouseAction<'Action> =
    | Screen of (Point   -> 'Action)
    | World  of (Vector2 -> 'Action)

type InputMouse<'Action> = {
    Camera:                Camera
    Buttons:               list<MouseButton * ButtonState * FMouseAction<'Action>>
    ScrollWheel:           option<int   -> 'Action>
    HorizontalScrollWheel: option<int   -> 'Action>
    Position:              option<Point -> 'Action>
}

type Input<'Action> = {
    Keyboard:   list<Input.Keys * ButtonState * 'Action>
    GamePad:    InputGamePad<'Action>
    Mouse:      InputMouse<'Action>
}

module FInput =
    let mapInput definition =
        let actions = ResizeArray<_>()

        // Keyboard Input Handling
        for button,state,action in definition.Keyboard do
            match state with
            | IsPressed  -> if FKeyboard.isPressed  button then actions.Add action
            | IsReleased -> if FKeyboard.isReleased button then actions.Add action
            | IsKeyDown  -> if FKeyboard.isKeyDown  button then actions.Add action
            | IsKeyUp    -> if FKeyboard.isKeyUp    button then actions.Add action

        // GamePad Buttons Handling
        for button,state,action in definition.GamePad.Buttons do
            match state with
            | IsPressed  -> if FGamePad.isPressed  button then actions.Add action
            | IsReleased -> if FGamePad.isReleased button then actions.Add action
            | IsKeyDown  -> if FGamePad.isKeyDown  button then actions.Add action
            | IsKeyUp    -> if FGamePad.isKeyUp    button then actions.Add action

        // GamePad ThumbStick Handling
        if FGamePad.stickLeft () <> Vector2.Zero then
            definition.GamePad.ThumbStick.Left |> Option.iter (fun f ->
                actions.Add (f (Vector2.flipY (FGamePad.stickLeft ())))
            )
        if FGamePad.stickRight () <> Vector2.Zero then
            definition.GamePad.ThumbStick.Right |> Option.iter (fun f ->
                actions.Add (f (Vector2.flipY (FGamePad.stickRight ())))
            )

        // GamePad Triggers
        if FGamePad.triggerLeft () |> notNearly 0.0f 0.0001f then
            definition.GamePad.Trigger.Left |> Option.iter (fun f ->
                actions.Add (f (FGamePad.triggerLeft ()))
            )
        if FGamePad.triggerRight () |> notNearly 0.0f 0.0001f then
            definition.GamePad.Trigger.Right |> Option.iter (fun f ->
                actions.Add (f (FGamePad.triggerRight ()))
            )

        // Mouse Handling
        let mouseAction action =
            match action with
            | Screen f -> f (FMouse.position ())
            | World  f -> f (Camera.screenPointToWorld (FMouse.position ()) definition.Mouse.Camera)

        for button,state,action in definition.Mouse.Buttons do
            match state with
            | IsPressed  -> if FMouse.isPressed  button then actions.Add (mouseAction action)
            | IsReleased -> if FMouse.isReleased button then actions.Add (mouseAction action)
            | IsKeyUp    -> if FMouse.isKeyUp    button then actions.Add (mouseAction action)
            | IsKeyDown  -> if FMouse.isKeyDown  button then actions.Add (mouseAction action)

        definition.Mouse.ScrollWheel |> Option.iter (fun f ->
            actions.Add (f (FMouse.scrollWheel ()))
        )
        definition.Mouse.HorizontalScrollWheel |> Option.iter (fun f ->
            actions.Add (f (FMouse.horizontalScrollWheel ()))
        )
        definition.Mouse.Position |> Option.iter (fun f ->
            actions.Add (f (FMouse.position ()))
        )

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
